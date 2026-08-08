# DreamLeague web

This directory contains an initial TypeScript port of the Shiny app in `dreamleague/app.R`.

## Data flow

The app reads `public/data/bundle.json`.

To regenerate that bundle from the existing R preprocessing flow:

1. Run `R/dl-preprocessing-run.R`
2. That script now calls `R/export-dreamleague-json.R`
3. JSON files are written to `web/public/data/`

## Run locally

Install dependencies in `web/`:

- `npm install`

Generate the latest JSON bundle from the existing R pipeline:

- run `source("R/export-dreamleague-json.R")`, or
- run `source("R/dl-preprocessing-run.R")` to refresh and export together

Start the dev server in `web/`:

- `npm run dev`

Then open http://127.0.0.1:4173

## First local validation checklist

After the app starts, check these first:

- the League tab loads standings for both leagues
- clicking a team opens the Teams tab with the same team selected
- the Teams tab shows a logo when the corresponding image exists in `web/public/img/`
- the Cup tab populates rounds when the competition changes
- the History tab shows rows for the default last-7-days range
- the Diagnostics tab either shows invalid squads or a no-issues message

If `npm run build` reports errors, the most likely remaining causes are environment-specific TypeScript or JSX checks rather than missing data.

## Current scope

This is the first implementation pass. It preserves the existing tabs and most core behavior, but it is not yet feature-complete relative to the Shiny widgets.
