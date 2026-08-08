import { Fragment, useEffect, useMemo, useState } from 'react';
import {
  formatTimestamp,
  getCupRows,
  getDiagnosticsRows,
  getHistoryRows,
  getLeagueRows,
  getPlayersTaken,
  getRoundLabel,
  getRounds,
  getScoreHistory,
  getTeamRows,
  getTeamSummary,
  getTeamsForLeague,
  loadBundle,
  teamToLogoCandidates,
} from './data';
import type { Bundle, CupCompetition, LeagueKey, TeamRow } from './types';

const tabs = [
  { key: 'league', label: 'League' },
  { key: 'teams', label: 'Teams' },
  { key: 'cup', label: 'BFL Cup' },
  { key: 'players', label: 'Players taken' },
  { key: 'history', label: 'History' },
  { key: 'diagnostics', label: 'Diagnostics' },
  { key: 'bug', label: 'Report an issue' },
] as const;

type TabKey = (typeof tabs)[number]['key'];

function toDateInputValue(date: Date): string {
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, '0');
  const day = String(date.getDate()).padStart(2, '0');
  return `${year}-${month}-${day}`;
}

function todayMinus(days: number): string {
  const date = new Date();
  date.setDate(date.getDate() - days);
  return toDateInputValue(date);
}

function App() {
  const [bundle, setBundle] = useState<Bundle | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [tab, setTab] = useState<TabKey>('league');
  const [league, setLeague] = useState<LeagueKey>('didsbury');
  const [team, setTeam] = useState<string>('');
  const [currentOnly, setCurrentOnly] = useState(true);
  const [comp, setComp] = useState<CupCompetition>('didsbury');
  const [round, setRound] = useState('');
  const [startDate, setStartDate] = useState(todayMinus(6));
  const [endDate, setEndDate] = useState(todayMinus(0));
  const [expandedTeamRow, setExpandedTeamRow] = useState<string | null>(null);
  const [expandedHistoryTeam, setExpandedHistoryTeam] = useState<string | null>(null);
  const [expandedCupIndex, setExpandedCupIndex] = useState<number | null>(null);
  const [playerSearch, setPlayerSearch] = useState('');
  const [teamPickerOpen, setTeamPickerOpen] = useState(false);

  useEffect(() => {
    loadBundle()
      .then((data) => setBundle(data))
      .catch((err: Error) => setError(err.message));
  }, []);

  const leagueRows = useMemo(() => (bundle ? getLeagueRows(bundle, league) : []), [bundle, league]);
  const teams = useMemo(() => (bundle ? getTeamsForLeague(bundle, league) : []), [bundle, league]);

  useEffect(() => {
    if (!team && teams.length > 0) {
      setTeam(teams[0].team);
      return;
    }

    if (team && teams.length > 0 && !teams.some((row) => row.team === team)) {
      setTeam(teams[0].team);
    }
  }, [team, teams]);

  useEffect(() => {
    setTeamPickerOpen(false);
  }, [league, team]);

  const rounds = useMemo(() => (bundle ? getRounds(bundle, comp) : []), [bundle, comp]);
  const selectedRound = round && rounds.includes(round) ? round : rounds[rounds.length - 1] ?? '';

  useEffect(() => {
    if (!round && rounds.length > 0) {
      setRound(rounds[rounds.length - 1]);
      return;
    }

    if (round && !rounds.includes(round)) {
      setRound(rounds[rounds.length - 1] ?? '');
    }
  }, [round, rounds]);

  if (error) {
    return <div className="app-shell"><p>{error}</p></div>;
  }

  if (!bundle) {
    return <div className="app-shell"><p>Loading DreamLeague data…</p></div>;
  }

  const selectedTeamOption = teams.find((item) => item.team === team) ?? null;
  const teamRows = team ? getTeamRows(bundle, league, team, currentOnly) : [];
  const teamSummary = team ? getTeamSummary(bundle, league, leagueRows, team) : null;
  const playersTaken = getPlayersTaken(bundle, league);
  const normalizedPlayerSearch = playerSearch.trim().toLocaleLowerCase();
  const filteredPlayersTaken = normalizedPlayerSearch
    ? playersTaken.filter((row) =>
      [row.team, row.player ?? '', row.club, row.position]
        .join(' ')
        .toLocaleLowerCase()
        .includes(normalizedPlayerSearch),
    )
    : playersTaken;
  const historyRows = getHistoryRows(bundle, league, startDate, endDate);
  const diagnosticsRows = getDiagnosticsRows(bundle);
  const cupRows = selectedRound ? getCupRows(bundle, comp, selectedRound) : [];
  const lastModified = league === 'didsbury' ? bundle.time.mod_d : bundle.time.mod_o;

  return (
    <div className="app-shell">
      <aside className="sidebar">
        <h1>DreamLeague</h1>
        <nav>
          {tabs.map((item) => (
            <button
              key={item.key}
              className={item.key === tab ? 'nav-button active' : 'nav-button'}
              onClick={() => setTab(item.key)}
            >
              {item.label}
            </button>
          ))}
        </nav>
      </aside>
      <main className="main-panel">
        {tab === 'league' && (
          <section className="tab-panel">
            <div className="panel-grid">
              <div className="controls-panel">
                <div className="timestamp-panel">
                  <h3>Last Updated</h3>
                  <p>Last score update: {formatTimestamp(bundle.time.update_time)}</p>
                  <p>Last file upload Didsbury: {formatTimestamp(bundle.time.mod_d)}</p>
                  <p>Last file upload Original: {formatTimestamp(bundle.time.mod_o)}</p>
                </div>
                <LeagueSelector league={league} onChange={setLeague} />
              </div>
              <div>
                <div className="banner warning">
                  Some goals may be missing due to changes in Soccerbase. Please{' '}
                  <a
                    className="inline-link-button"
                    href="#bug"
                    onClick={(event) => {
                      event.preventDefault();
                      setTab('bug');
                    }}
                  >
                    report an issue
                  </a>{' '}
                  so it can be fixed.
                </div>
                <table>
                  <thead>
                    <tr><th>Team</th><th>Manager</th><th>Total</th><th>For</th><th>Against</th></tr>
                  </thead>
                  <tbody>
                    {leagueRows.map((row) => (
                      <tr key={row.team} className={row.rank === 1 ? 'rank-first' : row.rank === 2 ? 'rank-second' : ''}>
                        <td>
                          <button
                            className="link-button"
                            onClick={() => {
                              setTeam(row.team);
                              setTab('teams');
                            }}
                          >
                            {row.team}
                          </button>
                        </td>
                        <td>{row.manager}</td>
                        <td>{row.total}</td>
                        <td>{row.gf}</td>
                        <td>{row.ga}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          </section>
        )}

        {tab === 'teams' && (
          <section className="tab-panel">
            <div className="panel-grid">
              <div className="controls-panel">
                <LeagueSelector league={league} onChange={setLeague} />
                <label>
                  Team
                  <div className="team-picker">
                    <button
                      type="button"
                      className="team-picker-trigger"
                      onClick={() => setTeamPickerOpen((open) => !open)}
                      aria-haspopup="listbox"
                      aria-expanded={teamPickerOpen}
                    >
                      <span className="team-picker-label">
                        {selectedTeamOption ? `${selectedTeamOption.team} (${selectedTeamOption.manager})` : 'Select team'}
                      </span>
                      <span className="team-picker-caret" aria-hidden="true">▾</span>
                    </button>
                    {teamPickerOpen && (
                      <div className="team-picker-menu" role="listbox" aria-label="Team">
                        {teams.map((item) => {
                          const selected = item.team === team;
                          return (
                            <button
                              key={item.team}
                              type="button"
                              role="option"
                              aria-selected={selected}
                              className={selected ? 'team-picker-option selected' : 'team-picker-option'}
                              onClick={() => {
                                setTeam(item.team);
                                setTeamPickerOpen(false);
                              }}
                            >
                              {item.team} ({item.manager})
                            </button>
                          );
                        })}
                      </div>
                    )}
                  </div>
                </label>
                <label className="checkbox-row">
                  <input type="checkbox" checked={currentOnly} onChange={(event) => setCurrentOnly(event.target.checked)} />
                  Current team only
                </label>
                {team && <TeamLogo team={team} />}
                {teamSummary && (
                  <div className="summary-box">
                    <p><strong>League position:</strong> {teamSummary.rank ?? '-'}</p>
                    <p><strong>Score:</strong> {teamSummary.total ?? '-'}</p>
                    <p><strong>For:</strong> {teamSummary.gf ?? '-'}</p>
                    <p><strong>Against:</strong> {teamSummary.ga ?? '-'}</p>
                    <p>Outfield transfers remaining: {teamSummary.outfieldTransfersRemaining}</p>
                    <p>Goalkeeper transfers remaining: {teamSummary.goalkeeperTransfersRemaining}</p>
                  </div>
                )}
                <button onClick={() => setTab('league')}>Return to League</button>
              </div>
              <div>
                <table>
                  <thead>
                    <tr><th></th><th>Player</th><th>Club</th><th>Position</th><th>Goals</th><th>Cost</th><th>Bought</th></tr>
                  </thead>
                  <tbody>
                    {teamRows.map((row) => {
                      const key = `${row.team}-${row.player ?? row.club}-${row.position}`;
                      const open = expandedTeamRow === key;
                      return (
                        <Fragment key={key}>
                          <tr>
                            <td><button onClick={() => setExpandedTeamRow(open ? null : key)}>{open ? '−' : '+'}</button></td>
                            <td>{row.player ?? ''}</td>
                            <td>{row.club}</td>
                            <td>{row.position}</td>
                            <td>{row.SBgoals}</td>
                            <td>{row.cost ?? ''}</td>
                            <td>{row.bought ?? ''}</td>
                          </tr>
                          {open && (
                            <tr className="detail-row">
                              <td colSpan={7}>
                                <ScoreHistory bundle={bundle} league={league} row={row} />
                              </td>
                            </tr>
                          )}
                        </Fragment>
                      );
                    })}
                  </tbody>
                </table>
              </div>
            </div>
          </section>
        )}

        {tab === 'players' && (
          <section className="tab-panel">
            <div className="panel-grid players-panel-grid">
              <div className="controls-panel compact">
                <LeagueSelector league={league} onChange={setLeague} />
                <label>
                  Search taken players
                  <input
                    type="search"
                    value={playerSearch}
                    onChange={(event) => setPlayerSearch(event.target.value)}
                    placeholder="Search team, player, club, position"
                  />
                </label>
                <p className="muted-text">
                  Showing {filteredPlayersTaken.length} of {playersTaken.length} players.
                </p>
              </div>
              <div>
                <div className="banner warning">This table was last updated on {formatTimestamp(lastModified)}; transfers since then will not be reflected here.</div>
                <table>
                  <thead>
                    <tr><th>Team</th><th>Player</th><th>Club</th><th>Position</th></tr>
                  </thead>
                  <tbody>
                    {filteredPlayersTaken.map((row) => (
                      <tr key={`${row.team}-${row.player ?? row.club}`}>
                        <td>{row.team}</td>
                        <td>{row.player ?? ''}</td>
                        <td>{row.club}</td>
                        <td>{row.position}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          </section>
        )}

        {tab === 'cup' && (
          <section className="tab-panel">
            <div className="panel-grid">
              <div className="controls-panel">
                <label>
                  Competition
                  <select value={comp} onChange={(event) => setComp(event.target.value as CupCompetition)}>
                    <option value="bfl">BFL Challenge Cup</option>
                    <option value="didsbury">Didsbury Cup</option>
                    <option value="original">Original Cup</option>
                  </select>
                </label>
                <label>
                  Round
                  <select value={selectedRound} onChange={(event) => setRound(event.target.value)}>
                    {rounds.map((item) => <option key={item} value={item}>{item}</option>)}
                  </select>
                </label>
                <p>{getRoundLabel(bundle, comp, selectedRound)}</p>
              </div>
              <div>
                <div className="banner info">Rows expand to show scorers.</div>
                <table>
                  <thead>
                    <tr><th></th><th></th><th></th><th></th><th></th></tr>
                  </thead>
                  <tbody>
                    {cupRows.map((row, index) => {
                      const open = expandedCupIndex === index;
                      return (
                        <Fragment key={`${row.team1}-${row.team2}`}>
                          <tr>
                            <td><button onClick={() => setExpandedCupIndex(open ? null : index)}>{open ? '−' : '+'}</button></td>
                            <td className={row.winner === 1 ? 'rank-first' : ''}>{row.teamManager1}</td>
                            <td className={row.winner === 1 ? 'rank-first' : ''}>{row.score1}</td>
                            <td className={row.winner === 2 ? 'rank-first' : ''}>{row.score2}</td>
                            <td className={row.winner === 2 ? 'rank-first' : ''}>{row.teamManager2}</td>
                          </tr>
                          {open && (
                            <tr className="detail-row">
                              <td colSpan={5}>
                                <strong>Scorers:</strong><br />
                                {row.team1}: {row.scorers1 || 'None'}<br />
                                {row.team2}: {row.scorers2 || 'None'}
                              </td>
                            </tr>
                          )}
                        </Fragment>
                      );
                    })}
                  </tbody>
                </table>
              </div>
            </div>
          </section>
        )}

        {tab === 'history' && (
          <section className="tab-panel">
            <div className="panel-grid">
              <div className="controls-panel">
                <LeagueSelector league={league} onChange={setLeague} />
                <label>
                  Start date
                  <input type="date" value={startDate} onChange={(event) => setStartDate(event.target.value)} />
                </label>
                <label>
                  End date
                  <input type="date" value={endDate} onChange={(event) => setEndDate(event.target.value)} />
                </label>
              </div>
              <div>
                <div className="banner info">Rows expand to show scorers; data defaults to the last 7 days.</div>
                <table>
                  <thead>
                    <tr><th></th><th>Team</th><th>Manager</th><th>Total</th><th>For</th><th>Against</th></tr>
                  </thead>
                  <tbody>
                    {historyRows.map((row) => {
                      const open = expandedHistoryTeam === row.team;
                      return (
                        <Fragment key={row.team}>
                          <tr>
                            <td><button onClick={() => setExpandedHistoryTeam(open ? null : row.team)}>{open ? '−' : '+'}</button></td>
                            <td>{row.team}</td>
                            <td>{row.manager}</td>
                            <td>{row.total}</td>
                            <td>{row.gf}</td>
                            <td>{row.ga}</td>
                          </tr>
                          {open && (
                            <tr className="detail-row">
                              <td colSpan={6}><strong>Scorers:</strong> {row.scorers || 'None'}</td>
                            </tr>
                          )}
                        </Fragment>
                      );
                    })}
                  </tbody>
                </table>
              </div>
            </div>
          </section>
        )}

        {tab === 'diagnostics' && (
          <section className="tab-panel">
            {diagnosticsRows.length === 0 ? (
              <div className="banner info">No squad composition issues found in the active squads.</div>
            ) : (
              <table>
                <thead>
                  <tr><th>Team</th><th>Goalkeeper</th><th>Defender</th><th>Midfielder</th><th>Forward</th></tr>
                </thead>
                <tbody>
                  {diagnosticsRows.map((row) => (
                    <tr key={row.team}>
                      <td>{row.team}</td>
                      <td>{row.GOALKEEPER ?? 0}</td>
                      <td>{row.DEFENDER ?? 0}</td>
                      <td>{row.MIDFIELDER ?? 0}</td>
                      <td>{row.FORWARD ?? 0}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            )}
          </section>
        )}

        {tab === 'bug' && (
          <section className="tab-panel">
            <iframe
              title="Report an issue"
              src="https://docs.google.com/forms/d/e/1FAIpQLScDhSXL2h8HYjTCuwdYKLTF3En2xPfE9O2BJet6VasuRdn2SQ/viewform?embedded=true"
              width="800"
              height="500"
            />
          </section>
        )}
      </main>
    </div>
  );
}

function LeagueSelector({ league, onChange }: { league: LeagueKey; onChange: (league: LeagueKey) => void }) {
  return (
    <fieldset className="league-selector">
      <legend>League</legend>
      <label className="radio-option"><input type="radio" name="league" value="didsbury" checked={league === 'didsbury'} onChange={() => onChange('didsbury')} /> <span>Didsbury</span></label>
      <label className="radio-option"><input type="radio" name="league" value="original" checked={league === 'original'} onChange={() => onChange('original')} /> <span>Original</span></label>
    </fieldset>
  );
}

function TeamLogo({ team }: { team: string }) {
  const [srcIndex, setSrcIndex] = useState(0);
  const [hidden, setHidden] = useState(false);
  const candidates = teamToLogoCandidates(team);

  useEffect(() => {
    setSrcIndex(0);
    setHidden(false);
  }, [team]);

  if (hidden) {
    return null;
  }

  return (
    <img
      className="team-logo"
      src={candidates[srcIndex]}
      alt={team}
      onError={() => {
        if (srcIndex < candidates.length - 1) {
          setSrcIndex((current) => current + 1);
        } else {
          setHidden(true);
        }
      }}
    />
  );
}

function ScoreHistory({ bundle, league, row }: { bundle: Bundle; league: LeagueKey; row: TeamRow }) {
  const history = getScoreHistory(bundle, league, row);
  if (history.length === 0) {
    return <div>No goals recorded.</div>;
  }

  return (
    <table className="nested-table">
      <thead>
        <tr><th>Date</th><th>Goals</th></tr>
      </thead>
      <tbody>
        {history.map((item) => (
          <tr key={`${item.date}-${item.goals}`}>
            <td>{item.date}</td>
            <td>{item.goals}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}

export default App;
