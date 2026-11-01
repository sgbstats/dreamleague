# dreamleague

## Runtime configuration

The Diagnostics tab can manually run `.github/workflows/dl-preprocessing.yml`
and refresh the app from Supabase after a successful run. Configure the
deployed Shiny runtime with `GITHUB_TOKEN`: a GitHub fine-grained access token
that has **Actions: read and write** access to `sgbstats/dreamleague`. The
token is only read from the runtime environment and must be stored as a
deployment secret, not in the repository.

Optionally set `DREAMLEAGUE_GITHUB_REPOSITORY` to override the default
`sgbstats/dreamleague` repository.

## Wenger Boys weekly report

`.github/workflows/wengerboys-weekly.yml` runs every Thursday at 01:00 UTC
(and on demand via **Run workflow**). `R/wengerboys-weekly.R` reads the
current Wenger Boys squad from the Supabase data bundle, scrapes Soccerbase for
the previous 7 days (goals, games, minutes, unused substitute / not in squad),
flags club moves (a note when the Soccerbase club does not fuzzy-match the club in the DreamLeague spreadsheet, and a red warning whenever the club is outside the 92), picks the best available 2 defenders, 3 midfielders and
5 forwards, emails the report through Resend and saves a JSON snapshot to
`wengerboys/<date>.json` in the Supabase bucket.

### One-off setup

1. **Resend account**: sign up at <https://resend.com>. Under **API Keys**
   choose **Create API Key** (permission: *Sending access*) and copy the
   `re_...` key.
2. **Sender**: with no domain, Resend only lets `onboarding@resend.dev` send
   to the email address you registered with. That is the default and needs no
   further setup. To use your own domain, add and verify it under **Domains**
   and set the repository variable `WENGERBOYS_EMAIL_FROM`
   (for example `Wenger Boys <report@yourdomain.com>`).
3. **GitHub secrets** (repo **Settings > Secrets and variables > Actions**):
   - `RESEND_API_KEY`: the key from step 1.
   - `WENGERBOYS_EMAIL_TO`: the address to deliver to (your Resend sign-up
     address when using `onboarding@resend.dev`).
   - `SUPABASE_API_KEY`: already used by the preprocessing workflow.
4. **Test**: **Actions > Wenger Boys weekly report > Run workflow**. Tick
   *dry_run* to build the report as a downloadable artifact without sending,
   or leave it unticked to send the email.

### Each season

Update `data/league92_clubs.csv` (Soccerbase `team_id`, club, division,
season) for promotions and relegations. The report warns at the top if the file
does not list exactly 92 clubs.

### Notes

- Competitions counted are the same as the DreamLeague scoring list.
- Minutes assume 90 for a full match and are derived from substitution times
  (stoppage time and extra time are not counted).
- Scouting ranks players not owned by any DreamLeague team by season goals,
  with fewer appearances as the tiebreak. The scouting scrape visits every
  candidate once, so the run takes a while.
- Local test: set `WENGERBOYS_BUNDLE_PATH` (local data bundle),
  `WENGERBOYS_DRY_RUN=true` and optionally `WENGERBOYS_SCOUT_LIMIT=40`, then run
  `Rscript R/wengerboys-weekly.R`.

## CI/CD

`.github/workflows/ci-cd.yml` runs `Rscript tests/testthat.R` on pushes and pull
requests targeting `staging` or `main`. The live Wenger Boys test is skipped in
CI. A successful push to `staging` deploys `dreamleague/` to the staging Posit
Connect Cloud content; a successful push to `main` deploys the production
content. Use `staging` as a protected pre-production branch: merge a tested
change there to deploy and verify the staging app, then promote it to `main`.
Pull requests from forks run tests but do not receive deployment credentials.

Create `staging` and `production` GitHub Actions environments and configure
these settings in each:

- Secrets `CONNECT_CLOUD_CLIENT_ID` and `CONNECT_CLOUD_CLIENT_SECRET` from a
  non-interactive Posit Connect Cloud credential.
- Secret `CONNECT_CLOUD_APP_ID` set to the existing staging or production
  content ID for that environment.
- Optional variable `CONNECT_CLOUD_ACCOUNT` (defaults to `sgbstats`).

The workflow uses these credentials only after tests pass, and targets the
existing content IDs rather than creating new apps. Configure environment
protection rules in GitHub if deployments should require approval, and protect
the `staging` branch so only reviewed changes can trigger deployment.