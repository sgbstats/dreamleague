import type {
  Bundle,
  CupCompetition,
  CupMatchRow,
  CupTieRow,
  DailyRow,
  DiagnosticsRow,
  HistoryRow,
  LeagueKey,
  LeagueRow,
  PlayersTakenRow,
  SquadPosition,
  TeamOption,
  TeamRow,
  TeamSummary,
} from './types';

const POSITION_ORDER: SquadPosition[] = ['GOALKEEPER', 'DEFENDER', 'MIDFIELDER', 'FORWARD'];

const LOGO_MODULES = import.meta.glob('../../dreamleague/img/*.{png,PNG}', {
  eager: true,
  import: 'default',
}) as Record<string, string>;

const LOGO_BY_TEAM = Object.fromEntries(
  Object.entries(LOGO_MODULES).map(([path, url]) => {
    const filename = path.split('/').pop() ?? '';
    const basename = filename.replace(/\.(png|PNG)$/u, '');
    return [basename.toUpperCase(), url];
  }),
) as Record<string, string>;

function isActive(row: { sold: string | null | undefined }): boolean {
  return row.sold == null || row.sold === '';
}

function byLeague<T extends { league: LeagueKey }>(rows: T[], league: LeagueKey): T[] {
  return rows.filter((row) => row.league === league);
}

function sum(values: number[]): number {
  return values.reduce((acc, value) => acc + value, 0);
}

function titleCase(value: string): string {
  return value
    .toLowerCase()
    .split(' ')
    .filter(Boolean)
    .map((part) => part[0]?.toUpperCase() + part.slice(1))
    .join(' ');
}

function parseDateValue(value: string | null | undefined): Date | null {
  if (!value) return null;

  const iso = new Date(value);
  if (!Number.isNaN(iso.getTime())) {
    return iso;
  }

  const dayMonthYear = value.match(/^(\d{4})-(\d{2})-(\d{2})$/);
  if (dayMonthYear) {
    const [, year, month, day] = dayMonthYear;
    return new Date(Number(year), Number(month) - 1, Number(day));
  }

  const dayMonthAbbr = value.match(/^(\d{2})-([A-Za-z]{3})-(\d{4})$/);
  if (dayMonthAbbr) {
    const [, day, monthText, year] = dayMonthAbbr;
    const monthLookup = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'];
    const monthIndex = monthLookup.indexOf(monthText.toLowerCase());
    if (monthIndex >= 0) {
      return new Date(Number(year), monthIndex, Number(day));
    }
  }

  return null;
}

function inActiveWindow(row: DailyRow, start: Date, end: Date): boolean {
  const date = parseDateValue(row.Date);
  if (!date || date < start || date > end) return false;
  const bought = parseDateValue(row.bought2 ?? null);
  const sold = parseDateValue(row.sold2 ?? null);
  if (bought && date < bought) return false;
  if (sold && date > sold) return false;
  return true;
}

export async function loadBundle(): Promise<Bundle> {
  const response = await fetch('/data/bundle.json');
  if (!response.ok) {
    throw new Error('Failed to load DreamLeague bundle');
  }
  return response.json() as Promise<Bundle>;
}

export function getLeagueRows(bundle: Bundle, league: LeagueKey): LeagueRow[] {
  const managers = byLeague(bundle.managers, league);
  const rows = byLeague(bundle.dl, league);

  const summaries = managers.map((manager) => {
    const teamRows = rows.filter((row) => row.team === manager.team);
    const total = sum(teamRows.map((row) => Number(row.SBgoals) || 0));
    const gf = sum(teamRows.filter((row) => row.position !== 'GOALKEEPER').map((row) => Number(row.SBgoals) || 0));
    const ga = -sum(teamRows.filter((row) => row.position === 'GOALKEEPER').map((row) => Number(row.SBgoals) || 0));
    return {
      league,
      team: manager.team,
      manager: manager.manager,
      total,
      gf,
      ga,
    };
  });

  return summaries
    .sort((a, b) => (b.total - a.total) || (b.gf - a.gf) || a.team.localeCompare(b.team))
    .map((row, index) => ({ ...row, rank: index + 1 }));
}

export function getTeamsForLeague(bundle: Bundle, league: LeagueKey): TeamOption[] {
  return byLeague(bundle.managers, league)
    .slice()
    .sort((a, b) => a.team.localeCompare(b.team))
    .map((row) => ({ team: row.team, manager: row.manager }));
}

export function getTeamRows(bundle: Bundle, league: LeagueKey, team: string, currentOnly: boolean): TeamRow[] {
  return bundle.dl
    .filter((row) => row.league === league && row.team === team)
    .filter((row) => (currentOnly ? isActive(row) : true))
    .map((row) => ({
      player: row.player,
      team: row.team,
      club: row.club,
      position: row.position,
      SBgoals: Number(row.SBgoals) || 0,
      cost: row.cost,
      bought: row.bought,
      sold: row.sold,
    }))
    .sort((a, b) => {
      const aIndex = POSITION_ORDER.indexOf(a.position as SquadPosition);
      const bIndex = POSITION_ORDER.indexOf(b.position as SquadPosition);
      return (aIndex - bIndex) || a.club.localeCompare(b.club);
    });
}

export function getTeamSummary(bundle: Bundle, league: LeagueKey, leagueRows: LeagueRow[], team: string): TeamSummary {
  const teamDl = bundle.dl.filter((row) => row.league === league && row.team === team);
  const leagueRow = leagueRows.find((row) => row.team === team);
  const outfieldTransfersRemaining = 8 - teamDl.filter((row) => row.position !== 'GOALKEEPER' && row.cost === '').length;
  const goalkeeperTransfersRemaining = 2 - teamDl.filter((row) => row.position === 'GOALKEEPER' && row.cost === '').length;

  return {
    rank: leagueRow?.rank,
    total: leagueRow?.total,
    gf: leagueRow?.gf,
    ga: leagueRow?.ga,
    outfieldTransfersRemaining,
    goalkeeperTransfersRemaining,
  };
}

export function getScoreHistory(bundle: Bundle, league: LeagueKey, teamRow: TeamRow): Array<{ date: string; goals: number }> {
  if (!teamRow.player) {
    return bundle.daily
      .filter((row) => row.league === league && row.team === teamRow.team && row.position === 'GOALKEEPER' && row.SBgoals !== 0)
      .map((row) => ({ date: row.Date, goals: row.SBgoals }))
      .sort((a, b) => b.date.localeCompare(a.date));
  }

  return bundle.daily
    .filter((row) => row.league === league && row.team === teamRow.team && row.player === teamRow.player && row.SBgoals !== 0)
    .map((row) => ({ date: row.Date, goals: row.SBgoals }))
    .sort((a, b) => b.date.localeCompare(a.date));
}

export function getPlayersTaken(bundle: Bundle, league: LeagueKey): PlayersTakenRow[] {
  return byLeague(bundle.dl, league)
    .filter((row) => isActive(row))
    .map((row) => ({
      team: row.team,
      player: row.player,
      club: row.club,
      position: row.position,
    }))
    .sort((a, b) => a.team.localeCompare(b.team) || (a.player ?? '').localeCompare(b.player ?? ''));
}

export function getHistoryRows(bundle: Bundle, league: LeagueKey, start: string, end: string): HistoryRow[] {
  const startDate = new Date(start);
  const endDate = new Date(end);
  const period = byLeague(bundle.daily, league).filter((row) => {
    const date = parseDateValue(row.Date);
    return Boolean(date && date >= startDate && date <= endDate);
  });

  const managers = byLeague(bundle.managers, league);

  return managers
    .map((manager) => {
      const teamRows = period.filter((row) => row.team === manager.team);
      const total = sum(teamRows.map((row) => Number(row.SBgoals) || 0));
      const gf = sum(teamRows.filter((row) => row.position !== 'GOALKEEPER').map((row) => Number(row.SBgoals) || 0));
      const ga = -sum(teamRows.filter((row) => row.position === 'GOALKEEPER').map((row) => Number(row.SBgoals) || 0));
      const scorers = teamRows
        .filter((row) => row.SBgoals !== 0)
        .reduce<Map<string, number>>((acc, row) => {
          const name = row.position === 'GOALKEEPER'
            ? row.club
            : ((row.player?.split(/\s+/) ?? []).slice(-1)[0] ?? row.club);
          acc.set(name, (acc.get(name) ?? 0) + row.SBgoals);
          return acc;
        }, new Map());

      const scorersText = [...scorers.entries()]
        .map(([name, goals]) => `${titleCase(name)}${goals === 1 ? '' : ` (${goals})`}`)
        .join(', ');

      return {
        team: manager.team,
        manager: manager.manager,
        total,
        gf,
        ga,
        scorers: scorersText,
      };
    })
    .sort((a, b) => (b.total - a.total) || (b.gf - a.gf) || a.team.localeCompare(b.team));
}

export function getDiagnosticsRows(bundle: Bundle): DiagnosticsRow[] {
  const counts = new Map<string, DiagnosticsRow>();
  bundle.dl
    .filter((row) => isActive(row))
    .forEach((row) => {
      const current = counts.get(row.team) ?? { team: row.team, GOALKEEPER: 0, DEFENDER: 0, MIDFIELDER: 0, FORWARD: 0 };
      if (POSITION_ORDER.includes(row.position as SquadPosition)) {
        const position = row.position as SquadPosition;
        current[position] = (current[position] ?? 0) + 1;
      }
      counts.set(row.team, current);
    });

  return [...counts.values()].filter((row) =>
    row.GOALKEEPER !== 1 || row.DEFENDER !== 2 || row.MIDFIELDER !== 3 || row.FORWARD !== 5,
  );
}

function getCupRoundDate(cupties: CupTieRow[], comp: CupCompetition, round: string): Date | null {
  const roundDate = cupties
    .filter((row) => row.comp === comp && row.round === round)
    .map((row) => parseDateValue(row.date))
    .filter((value): value is Date => value instanceof Date)
    .sort((a, b) => a.getTime() - b.getTime())[0];

  return roundDate ?? null;
}

export function getRounds(bundle: Bundle, comp: CupCompetition): string[] {
  return [...new Set(bundle.cupties.filter((row) => row.comp === comp).sort((a, b) => a.date.localeCompare(b.date)).map((row) => row.round))];
}

export function getRoundLabel(bundle: Bundle, comp: CupCompetition, round: string): string {
  const start = getCupRoundDate(bundle.cupties, comp, round);
  if (!start) return '';
  const end = new Date(start);
  end.setDate(end.getDate() + 3);
  const sameMonth = start.getMonth() === end.getMonth();
  const startText = sameMonth
    ? `${String(start.getDate()).padStart(2, '0')}`
    : start.toLocaleDateString('en-GB', { day: '2-digit', month: 'short' });
  const endText = end.toLocaleDateString('en-GB', { day: '2-digit', month: 'short' });
  return `Round date: ${startText}-${endText}`;
}

export function getCupRows(bundle: Bundle, comp: CupCompetition, round: string): CupMatchRow[] {
  const league: LeagueKey | null = comp === 'didsbury' ? 'didsbury' : comp === 'original' ? 'original' : null;
  const start = getCupRoundDate(bundle.cupties, comp, round);
  if (!start) return [];

  const end = new Date(start);
  end.setDate(end.getDate() + 3);

  const weekend = bundle.daily.filter((row) => (league ? row.league === league : true) && inActiveWindow(row, start, end));

  const scoreMap = new Map<string, { total: number; gf: number; ga: number; scorers: string; manager: string }>();

  bundle.managers
    .filter((manager) => (league ? manager.league === league : true))
    .forEach((manager) => {
      const teamRows = weekend.filter((row) => row.team === manager.team);
      const total = sum(teamRows.map((row) => Number(row.SBgoals) || 0));
      const gf = sum(teamRows.filter((row) => row.position !== 'GOALKEEPER').map((row) => Number(row.SBgoals) || 0));
      const ga = -sum(teamRows.filter((row) => row.position === 'GOALKEEPER').map((row) => Number(row.SBgoals) || 0));

      const scorerMap = teamRows
        .filter((row) => row.SBgoals !== 0)
        .reduce<Map<string, number>>((acc, row) => {
          const key = row.position === 'GOALKEEPER'
            ? row.club
            : ((row.player?.split(/\s+/) ?? []).slice(-1)[0] ?? row.club);
          acc.set(key, (acc.get(key) ?? 0) + row.SBgoals);
          return acc;
        }, new Map());

      scoreMap.set(manager.team, {
        total,
        gf,
        ga,
        scorers: [...scorerMap.entries()].map(([name, goals]) => `${titleCase(name)}${goals === 1 ? '' : ` (${goals})`}`).join(', '),
        manager: manager.manager,
      });
    });

  return bundle.cupties
    .filter((row) => row.comp === comp && row.round === round)
    .map((tie) => {
      const a = scoreMap.get(tie.team1) ?? { total: 0, gf: 0, ga: 0, scorers: '', manager: '' };
      const b = scoreMap.get(tie.team2) ?? { total: 0, gf: 0, ga: 0, scorers: '', manager: '' };
      const winner = a.total > b.total ? 1 : a.total < b.total ? 2 : a.gf > b.gf ? 1 : a.gf < b.gf ? 2 : null;
      return {
        team1: tie.team1,
        team2: tie.team2,
        teamManager1: `${tie.team1} (${a.manager})`,
        teamManager2: `${tie.team2} (${b.manager})`,
        score1: `${a.total} (${a.gf}-${a.ga})`,
        score2: `${b.total} (${b.gf}-${b.ga})`,
        winner,
        scorers1: a.scorers,
        scorers2: b.scorers,
      };
    });
}

export function formatTimestamp(value: string | null | undefined): string {
  if (!value) return '';
  return value.replace(/(\d{2}:\d{2}:\d{2})\.\d+/, '$1');
}

export function teamToLogoCandidates(team: string): string[] {
  const normalized = team.replace(/[^\p{L}\p{N}]/gu, '').toUpperCase();
  const bundled = LOGO_BY_TEAM[normalized];
  return bundled ? [bundled] : [];
}
