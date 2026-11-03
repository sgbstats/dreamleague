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

### Promotion and review

1. Push a feature branch. After R, Supabase and GitHub PAT tests pass, the workflow opens or reuses a PR
   into `staging` and enables auto-merge for the tested commit. If the branch is
   behind `staging` (each promotion adds a merge commit there), the workflow
   merges `staging` into the feature branch instead; that push is retested and
   then promoted. Pull before pushing again locally.
2. GitHub runs the staging PR tests against its merge result. Branch protection
   requires passing **Run R tests**, an up-to-date branch, and resolved review
   conversations. Conflicts or failed checks stop promotion; no bypass is used.
3. Once the PR merges, the `staging` push runs tests again and deploys the
   existing staging Posit Connect Cloud app. A successful HTTP check publishes
   **Open staging deployment** in the Actions run summary and the GitHub
   environment. This checks HTTP availability, not Shiny session functionality.
4. Open that link and review the app. **Stop here until explicitly instructed
   to release.** CI never creates or auto-merges a PR into `main`. After approval,
   open a PR from `staging` to `main`, wait for tests, and merge it manually.
   The resulting `main` push deploys production after tests pass.

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

| Name | Kind | Value |
|------|------|-------|
| `CONNECT_CLOUD_CLIENT_ID` | Secret | Client ID from a Posit Connect Cloud credential |
| `CONNECT_CLOUD_CLIENT_SECRET` | Secret | Matching client secret, shown only once when created |
| `CONNECT_CLOUD_APP_ID` | Secret | Existing content ID for this environment, not the old shinyapps.io app ID |
| `CONNECT_CLOUD_ACCOUNT` | Variable | Posit account name; defaults to `sgbstats` |
| `CONNECT_CLOUD_APP_URL` | Variable | Existing app's HTTPS `share.connect.posit.cloud` URL; required for staging preview |

Create the client credential at <https://login.posit.cloud/identity/credentials>.
Confirm the account name at <https://connect.posit.cloud/whoami>. Find each
content ID and app URL in its Connect Cloud content page. Use separate
credentials where possible. The workflow updates existing content and does not
create a replacement app.

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

Disable **Automatically publish on push** for both apps in Connect Cloud so
independent publishing cannot bypass this pipeline's test gate.

### App runtime secrets

Deployment credentials above authenticate CI; the app also needs runtime
variables. rsconnect replaces a Connect Cloud app's variables on every deploy,
so values set only in the Connect Cloud UI are wiped. The deploy step sends
them from GitHub on each deploy instead:

| Variable | GitHub source | Required |
|----------|---------------|----------|
| `GOOGLE_PRIVATE_KEY` | secret | yes |
| `SUPABASE_API_KEY` | secret | yes |
| `ACTION_TOKEN` | secret (fine-grained token, **Actions: read and write**, for the Diagnostics refresh button) | yes, CI permission check |
| `DREAMLEAGUE_SHARED_DRIVE_TARGET` | secret | no |
| `DREAMLEAGUE_SUPABASE_BUCKET` | secret | no |
| `SUPABASE_URL` | variable | no |

Repository-level secrets apply to both apps; add a secret with the same name to
the `staging` or `Production` environment to override it for one app. Missing
required secrets fail the deploy before contacting Connect Cloud. Optional
values left unset are removed from the app, so add every runtime value the app
needs to GitHub rather than to Connect Cloud. These values belong in GitHub
secrets, never in source files.

### GitHub PAT permission test

Add `ACTION_TOKEN` under **Settings > Secrets and variables > Actions > Repository
secrets**, or in each deployment environment (`staging` and `Production`).
The live check uses the target environment, matching the credential sent to the
app by deployment. Use a fine-grained PAT scoped to `sgbstats/dreamleague` with **Actions:
read and write** (and the automatically included Metadata read permission).
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