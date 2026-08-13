# Buffalo Bioeconomic Model

Decision-support tool for the financial feasibility of dairy buffalo (Murrah) enterprises in
North-West India (Haryana / Punjab / western UP). Produces bank-ready projections — NPV, IRR,
BCR, DSCR, payback, pre- and post-tax — for herds of 5–1,000 adult buffaloes over 5- or
10-year horizons.

Partners: Adelaide University · ICAR-CIRB Hisar · LUVAS
Live app: https://khatkar.shinyapps.io/buffalo_planner/
Project site: https://msk99.github.io/buffalo_econ/

## How it works

The engine couples three sub-models, driven entirely by one editable parameter workbook:

- **Biological** — a monthly cohort herd projection: reproduction on a single calving-interval
  clock, parity-dependent yield, young-stock rearing pipelines, and a fixed-herd policy in
  which culling balances the herd at its target size.
- **Technological** — the production requirements of a herd of a given size: ration build-up
  by animal class, labour in fractional full-time equivalents, housing area, and an equipment
  capacity model. All quantities move continuously with herd size (log-interpolated between
  two expert-entered anchor sizes), so results have no jumps at arbitrary size boundaries.
- **Economic** — prices the quantities, assembles the operating statement, applies Indian
  business taxation (entity forms, s.44AD presumptive scheme, slab rates — all held in the
  workbook), structures the bank loan, and computes every viability metric from one cash-flow
  timeline.

## Installation

Requires **R ≥ 4.2** (tested on 4.5). Clone, install dependencies, and the setup
script runs a self-check:

```bash
git clone https://github.com/msk99/buffalo_econ.git
cd buffalo_econ
Rscript setup.R
```

`setup.R` installs any missing packages (`data.table`, `openxlsx2`, `shiny`, `bslib`,
`DT`, `ggplot2`, `testthat`) from CRAN and runs one model projection to confirm the
installation works. No compilation and no system dependencies beyond R itself.

## Running

The interactive app:

```bash
R -e 'shiny::runApp("app", launch.browser = TRUE)'
```

The engine headlessly:

```bash
R -q -e 'for (f in list.files("R", full.names=TRUE)) source(f); r <- run_buffalo_model(50, 10); print(r$summary)'
```

The test suite (169 tests):

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

## Repository layout

```
├── R/                          The calculation engine, one module per concern
│   ├── params_load.R           Workbook -> typed object; anchors read from headers
│   ├── params_validate.R       Schema, range and cross-consistency checks; fails loudly
│   ├── params_scale.R          scale_mode -> values at one herd size; ration; equipment
│   ├── herd_cohort.R           Monthly cohorts, parity, fixed-herd culling policy
│   ├── ration.R / cost_*.R     Feed, labour, health, capital
│   ├── depreciation.R          Book (straight-line) and tax (WDV, s.36(1)(vi)) schedules
│   ├── revenue.R / pnl.R       Revenue lines, operating statement
│   ├── tax.R                   Entity forms, s.44AD with cap, slabs from the workbook
│   ├── finance_loan.R          Loan, moratorium, subsidy, debt service
│   ├── finance_viability.R     NPV / IRR / BCR / payback / DSCR on one timeline
│   ├── sensitivity.R           Tornado and breakeven solvers
│   ├── report_excel.R          Bankable Excel report (tempfile-safe)
│   └── run_model.R             Orchestrator
├── app/                        Shiny application (bslib) — all defaults from the workbook
├── params/
│   └── parameters_v3_2026-08.xlsx   The parameter workbook: 116 parameters, 14 sheets,
│                                    schema included. The single source of truth.
├── tests/testthat/             169 tests: params, herd, economics, finance, outputs
├── deploy/build_and_deploy.R   Builds a flat bundle; deploys only with BUFFALO_DEPLOY=yes
├── setup.R                     Dependency install + self-check
└── CITATION.cff                How to cite this software
```

## The parameter workbook

`params/parameters_v3_2026-08.xlsx` is the single source of truth — every parameter carries a
label, unit, basis, scaling behaviour, validation range and source note, alongside the ration
build-up, the equipment capacity model and the tax schedule. Domain experts edit the workbook,
never the code. The loader validates it against its own schema sheet and refuses to run on a
workbook that violates it. In the app, every sidebar default is resolved from the workbook at
the chosen herd size; user overrides are recorded and listed in the downloaded report.

## Validation status

The engine is fully built and internally verified (conservation, continuity, tax and
timeline-consistency properties are all under automated test). Reconciliation against real
reference project reports (a NABARD model report and an institute farm dataset) is still in
progress; until that is signed off, treat outputs as methodologically sound but not yet
bank-ready.

## Deployment

`Rscript deploy/build_and_deploy.R` builds a self-contained bundle in `deploy/bundle/`.
Publishing to shinyapps.io requires `SHINYAPPS_TOKEN` / `SHINYAPPS_SECRET` in `~/.Renviron`
and the explicit flag `BUFFALO_DEPLOY=yes`.

## License

Not yet licensed — all rights reserved pending agreement between the project partners
(Adelaide University, ICAR-CIRB Hisar, LUVAS). You may read and clone the code; a
license permitting reuse will be added once agreed. Citation details are in
`CITATION.cff`.
