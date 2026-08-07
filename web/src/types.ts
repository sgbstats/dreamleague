export type LeagueKey = 'didsbury' | 'original';
export type CupCompetition = 'bfl' | 'didsbury' | 'original';

export interface DlRow {
  league: LeagueKey;
  team: string;
  player: string | null;
  club: string;
  position: string;
  cost: string | number | null;
  goals?: number | null;
  SBgoals: number;
  bought: string | null;
  sold: string | null;
  bought2: string | null;
  sold2: string | null;
  SBapp?: number | null;
}

export interface DailyRow {
  league: LeagueKey;
  team: string;
  player: string | null;
  club: string;
  position: string;
  SBgoals: number;
  Date: string;
  bought2?: string | null;
  sold2?: string | null;
}

export interface ManagerRow {
  league: LeagueKey;
  manager: string;
  team: string;
}

export interface CupTieRow {
  comp: CupCompetition;
  round: string;
  team1: string;
  team2: string;
  date: string;
}

export interface TimeInfo {
  update_time: string;
  mod_d: string;
  mod_o: string;
}

export interface Bundle {
  dl: DlRow[];
  daily: DailyRow[];
  time: TimeInfo;
  cupties: CupTieRow[];
  managers: ManagerRow[];
}

export interface LeagueRow {
  league: LeagueKey;
  rank: number;
  team: string;
  manager: string;
  total: number;
  gf: number;
  ga: number;
}

export interface TeamRow {
  player: string | null;
  team: string;
  club: string;
  position: string;
  SBgoals: number;
  cost: string | number | null;
  bought: string | null;
  sold: string | null;
}

export interface ScoreHistoryRow {
  date: string;
  goals: number;
}

export interface PlayersTakenRow {
  team: string;
  player: string | null;
  club: string;
  position: string;
}

export interface TeamSummary {
  rank?: number;
  total?: number;
  gf?: number;
  ga?: number;
  outfieldTransfersRemaining: number;
  goalkeeperTransfersRemaining: number;
}

export interface TeamOption {
  team: string;
  manager: string;
}

export interface HistoryRow {
  team: string;
  manager: string;
  total: number;
  gf: number;
  ga: number;
  scorers: string;
}

export type SquadPosition = 'GOALKEEPER' | 'DEFENDER' | 'MIDFIELDER' | 'FORWARD';

export interface DiagnosticsRow {
  team: string;
  GOALKEEPER?: number;
  DEFENDER?: number;
  MIDFIELDER?: number;
  FORWARD?: number;
}

export interface CupMatchRow {
  team1: string;
  team2: string;
  teamManager1: string;
  teamManager2: string;
  score1: string;
  score2: string;
  winner: 1 | 2 | null;
  scorers1: string;
  scorers2: string;
}
