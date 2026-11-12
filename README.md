# dreamleague

## Runtime configuration

The Diagnostics tab can manually run `.github/workflows/dl-preprocessing.yml`
and refresh the app from Supabase after a successful run. Configure the
deployed Shiny runtime with `ACTION_TOKEN` (the app also accepts
`GITHUB_TOKEN`): a GitHub fine-grained access token that has **Actions: read and
write** access to `sgbstats/dreamleague`. Store it as the `ACTION_TOKEN` GitHub
secret; the CI/CD deploy passes it to the app (see **App runtime secrets**). It
must never be stored in the repository.

The app uses the default `sgbstats/dreamleague` repository;
`DREAMLEAGUE_GITHUB_REPOSITORY` can override it when running locally.

### Data retrieval and diagnostics

The app retrieves `data.RDa` from Supabase first, then Google Drive if Supabase
fails. It never loads a bundled/local data file. If both sources fail, the site
stays available with **No data has been retrieved. Please try again later.**
and a retry button. Failed manual refreshes also clear the displayed data;
data-dependent views remain unavailable until a remote load succeeds.
Successfully retrieved remote bundles are accepted without an age cutoff.

Diagnostics provides **Download data ZIP** only while data is available.
The archive contains the currently loaded `dl`, `daily`, `time` and `cupties`
objects in `data.RDa`, CSVs for tabular objects, and `metadata.txt` with the
source and retrieval time in UTC. No credentials, configuration, local
workbooks or other files are included.

Scheduled and manual preprocessing actions run
`Rscript R/dl-preprocessing-run.R -f`. Normal preprocessing always downloads
both XLSX inputs from Google Drive, regardless of local timestamps, and fails
before processing or publication if either download fails. `-f`/`--force` is
retained as an explicit remote-only flag; local files are never a fallback.
For each workbook, discovery includes numbered copies such as
`DreamLeague26-27 (1).xlsx` and `DL26-27 (2).xlsx`. The most recently modified
matching file is selected across the original and all numbered copies, not
just when the original filename is absent.
For testing with checked-out workbooks, run
`Rscript R/dl-preprocessing-run.R --local-test`. This explicit mode uses local
XLSX inputs and does not publish to Supabase or Google Drive. It cannot be
combined with `-f` or used in GitHub Actions. The file-check diagnostic script
also requires remote XLSX by default and accepts `--local-test` for testing.
Local testing still runs the Soccerbase processing steps.

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
- A Soccerbase player-page red-card marker adds a dismissal note, distinguishing
  a second yellow from a direct red. Match minutes use the lineup's `(s/o N)`
  sending-off minute minus the player's entry minute; otherwise they are blank
  and excluded from the minutes total rather than estimated from substitutions.
- Scouting ranks players not owned by any DreamLeague team by season goals,
  with fewer appearances as the tiebreak. The scouting scrape visits every
  candidate once, so the run takes a while.
- Local test: set `WENGERBOYS_BUNDLE_PATH` (local data bundle),
  `WENGERBOYS_DRY_RUN=true` and optionally `WENGERBOYS_SCOUT_LIMIT=40`, then run
  `Rscript R/wengerboys-weekly.R`.

## CI/CD

`.github/workflows/ci-cd.yml` runs `Rscript tests/testthat.R` on pushes and pull
requests targeting `staging` or `main`. The live Wenger Boys test is skipped in
CI; preprocessing-bundle validation skips when no preprocessing bundle is
supplied. The credential-free suite also skips the live Supabase test, which
runs separately in **Test Supabase round trip** on repository pushes and manual
runs. A separate **Test app GitHub PAT permissions** job verifies `ACTION_TOKEN`
on `staging`/`main` pushes and manual runs on those branches. All jobs must pass
(or be skipped as not needed) before automatic
staging promotion or deployment.
The existing suite checks app helpers and data contracts, not browser-level
Shiny interaction.

### Test sets

Test files are tagged by filename prefix, and CI runs only the sets affected by
the changed files (`.github/scripts/select-test-sets.R`):

| Set | Test files | Runs when these change |
|-----|------------|------------------------|
| app | `test-app-*.R` | `dreamleague/` |
| pipeline | `test-pipeline-*.R` | `R/`, `data/`, `.github/`, test fixtures |
| both | `test-shared-*.R` | either set's paths |

Changes to `tests/testthat.R`, test helpers or shared tests, manual runs, and
pushes whose history cannot be diffed run everything. Documentation-only changes
run no R test sets, but **Run R tests** still reports success for branch protection.
The deployment PAT check still runs on `staging` and `main`, even for documentation changes.
The live Supabase job runs only for changes to the app, Supabase code or
`ci-cd.yml`. New test files must use one of the three prefixes; a pipeline test
enforces this. Run a set locally with
`DREAMLEAGUE_TEST_SET=app Rscript tests/testthat.R` (`app`, `pipeline` or `all`,
the default).

CI installs R dependencies explicitly; the app does not use a repository-managed
renv library. Do not add a startup hook for `renv/activate.R` without committing
and configuring the corresponding renv project. The app-directory startup test
catches broken R startup before staging promotion.

The Docker build installs `libuv1-dev` for the `fs` package and verifies that
every required R package can load after installation. Missing or unloadable
packages fail the image build instead of producing an image that crashes at
startup; `install.packages()` warnings alone do not fail an R process.

### Promotion and review

1. Push a feature branch. After R, Supabase and GitHub PAT tests pass, the workflow opens or reuses a PR
   into `staging` and enables auto-merge for the tested commit. If the branch is
   behind `staging` (each promotion adds a merge commit there), the workflow
   merges `staging` into the feature branch instead; that push is retested and
   then promoted. Pull before pushing again locally.
2. GitHub runs the staging PR tests against its merge result. Branch protection
   requires passing **Run R tests**, an up-to-date branch, and resolved review
   conversations. Conflicts or failed checks stop promotion; no bypass is used.
3. Once the PR merges, the `staging` push runs tests again. Render automatically
   deploys the matching commit to `dreamleague-staging`; the workflow waits for
   that exact commit to reach Render's `live` status before publishing
   **Open staging deployment** in the Actions run summary and GitHub environment.
   This checks deployment status, not browser-level Shiny functionality.
4. Open that link and review the app. **Stop here until explicitly instructed
   to release.** CI never creates or auto-merges a PR into `main`. After approval,
   open a PR from `staging` to `main`, wait for tests, and merge it manually.
   The resulting `main` push waits for the GitHub **Production** environment
   approval, triggers a Render deploy for that exact commit, and waits until
   Render reports it live.

Actions cannot open a browser on your computer or this Copilot conversation.
Use the staging link in the run summary; Copilot can open it when asked once
the deployment is available. All feature-branch pushes in this repository are
eligible for automatic staging promotion, so only trusted contributors should
have repository write access. Fork PRs run tests without promotion credentials.
Staging is shared: a later feature can replace the preview, so check the commit
in the deployment summary before authorizing release.

### Credentials and one-off setup

In [repository Settings > Environments](https://github.com/sgbstats/dreamleague/settings/environments),
open **staging** and the existing **Production** environment. Use **Add
environment secret** or **Add environment variable** for the following:

| Environment | Name | Kind | Value |
|-------------|------|------|-------|
| `staging` | `RENDER_API_KEY` | Secret | Render API key with access to read service deployment status |
| `staging` | `RENDER_STAGING_SERVICE_ID` | Variable | Service ID for `dreamleague-staging`, from its Render dashboard URL/settings |
| `staging` | `RENDER_STAGING_URL` | Variable | The service's HTTPS `onrender.com` URL |
| `Production` | `RENDER_API_KEY` | Secret | Render API key with access to list services, deploy `dreamleague`, and read deployment status |

The production workflow locates the Render service named `dreamleague` through
the API, triggers a deploy for the exact commit, and polls its status. The
Production environment's existing required-reviewer approval remains the
release gate.

The root `render.yaml` adopts the existing Render Web Services by their exact
names. It deliberately omits compute plans so existing tiers are preserved,
declares runtime variable names with `sync: false`, and disables production
auto-deploy. Create/connect one Render Blueprint for this repository and review
the proposed changes carefully before syncing; it must show updates to the
existing services, not new duplicate services. Disable Blueprint **Auto Sync**
after the initial sync so configuration changes cannot bypass the production
approval workflow. The app services' secret values remain in each service's
Render environment configuration. Both current services are in Oregon, which
is recorded in the Blueprint.

For staging, set the three staging values above from the Render service
dashboard. Complete these GitHub settings and sync the Blueprint before merging
the first staging-to-main release PR; otherwise the workflow cannot confirm the
staging deployment or trigger and confirm the approved production deploy. The
GitHub Actions environment variables are separate from the app runtime variables
stored in Render.

In [Settings > Secrets and variables > Actions](https://github.com/sgbstats/dreamleague/settings/secrets/actions),
add repository secret **`STAGING_PROMOTION_TOKEN`**. Use a fine-grained GitHub
personal access token limited to this repository with **Contents: read and
write**, **Pull requests: read and write**, and **Workflows: read and write**
(needed when promoting workflow changes). The token owner must have write
access. A personal token is needed because merges and PRs created with the
default Actions `GITHUB_TOKEN` do not trigger the follow-on test/deploy workflows.
Do not reuse the app's runtime token for promotion.

Enable **Allow auto-merge** in repository Settings > General (already enabled
for this repository). Bootstrap by configuring the credentials and pushing the
pipeline feature branch, which proposes the initial workflow to `staging`.
If that initial promotion needs manual recovery, open a PR from the pipeline
feature branch to `staging` and merge it after tests pass. **Do not merge the
pipeline into `main` first**: preview staging and wait for release approval.
Normal feature branches should start from `staging` so they inherit the
workflow. Both branches require PRs and passing tests, block deletion and force
pushes, and enforce rules for admins. There is no required approving review
count, allowing a sole maintainer to promote changes. Environment policies
allow only `staging` to deploy staging and only `main` to deploy Production.

The active workflow no longer deploys to Posit. The existing `.posit/publish`
settings and deployment helper are retained only as rollback artifacts; leave
them inactive unless intentionally rolling back.

### App runtime secrets

Configure these variables separately in the Render environment settings for
**both** services. The Blueprint declares their names without values, so it
does not overwrite the values already configured in Render:

| Variable | Where to configure | Required |
|----------|--------------------|----------|
| `GOOGLE_PRIVATE_KEY` | Render secret | yes |
| `SUPABASE_API_KEY` | Render secret | yes |
| `ACTION_TOKEN` | Render secret; also a GitHub environment secret for CI permission checks | yes |
| `DREAMLEAGUE_SHARED_DRIVE_TARGET` | Render environment variable | no |
| `DREAMLEAGUE_SUPABASE_BUCKET` | Render environment variable | no |
| `SUPABASE_URL` | Render environment variable | no |

Keep the app's secret values in Render, never in source files. `ACTION_TOKEN`
and any credentials used by live GitHub/Supabase CI tests must also remain in
the relevant GitHub Actions environment because those tests use them directly.
The old `CONNECT_CLOUD_*` deployment credentials are no longer used by the
active workflow; they can be retained temporarily for rollback and removed
after the migration is accepted.

### Production domain: Cloudflare and Render

The Blueprint registers `dreamleague.uk` on the production service. Render
automatically adds `www.dreamleague.uk` and redirects it to the apex domain.
The Render-generated production URL remains enabled as a fallback; staging
continues to use its default `onrender.com` URL.

After the Blueprint is synced:

1. In the Render `dreamleague` service dashboard, confirm `dreamleague.uk` is
   listed under **Custom Domains** and copy the service's `onrender.com`
   hostname.
2. In Cloudflare, open **SSL/TLS > Overview** and set encryption mode to
   **Full**. Under **DNS > Records**, remove any `AAAA` records for
   `dreamleague.uk`.
3. Add CNAME records for both `@` and `www`, each targeting the production
   service's `onrender.com` hostname. Keep both records **DNS only**.
4. Wait for DNS propagation, then click **Verify** under the Render service's
   **Custom Domains** settings. Render issues and renews the TLS certificate;
   wait until it is reported valid before using the custom domain.

### GitHub PAT permission test

Add `ACTION_TOKEN` under **Settings > Secrets and variables > Actions > Repository
secrets**, or in each deployment environment (`staging` and `Production`).
The live check uses the target environment's `ACTION_TOKEN`, which must match
the token configured for that Render service. Use a fine-grained PAT scoped to
`sgbstats/dreamleague` with **Actions: read and write** (and the automatically
included Metadata read permission).
A classic PAT needs `repo` for a private repository or `public_repo` for a public
repository. The token must be unexpired and approved/SSO-authorized where required.
The token used to sign into Copilot or `gh` is not automatically available to
the deployed app or Actions.

The shared test checks workflow and run-list read access, then calls the same
dispatch endpoint as the app using an impossible Git ref containing `~`.
Only the specific HTTP 422 "No ref found" response passes: GitHub has authorized
the request, but cannot start data collection. Missing tokens, HTTP 401/403/404,
disabled workflows and other validation errors fail rather than skip. It does
not infer write access from scope headers, which fine-grained PATs do not expose.
The live probe only runs in its dedicated CI job, never in credential-free local
tests, feature-branch runs or PR events. No collection run is started.

### Supabase integration test

The **Test Supabase round trip** job uses repository secret `SUPABASE_API_KEY`
(the existing preprocessing credential) and repository variables `SUPABASE_URL`
and `DREAMLEAGUE_SUPABASE_TEST_BUCKET`. The URL defaults to the current project;
the test bucket defaults to `DREAMLEAGUE_SUPABASE_BUCKET`, then `dreamleague`.
Configure these in **Settings > Secrets and variables > Actions**. The key must
allow uploading, reading and deleting test objects. Missing credentials fail
this job rather than silently skipping it.

The test generates synthetic `dl`, `daily`, `time` and `cupties` objects in an R
temporary file. It uploads only to a unique `tests/roundtrip-*.RDa` path,
downloads using the app's storage helper, checks bytes and all object contents,
and deletes that exact remote object using the Storage bulk-delete endpoint.
Cleanup errors are reported, not ignored. It never uploads to the app's
`dreamleague/data.RDa` object and never needs a local copy of production data.
The app's data file stays gitignored and continues to be pulled from Supabase.

Use a separate test bucket and a scoped test credential when available. This
job runs only on repository pushes or manual workflow runs, not on PR events,
so fork PRs never receive the credential. Repository write access is therefore
a trust boundary: feature pushes execute code with this credential.