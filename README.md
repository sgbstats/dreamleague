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