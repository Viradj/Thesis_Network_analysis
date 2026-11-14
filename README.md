# Interlocking directors and fraud in Dutch healthcare companies

R code from my MSc thesis in Sociology (Utrecht University, 2021),
written during a research internship at Kurtosis.

## The question
Since 2015, Dutch municipalities are responsible for social support
(Wmo) and youth care, and healthcare fraud in their region has become
a concern. This research asks whether fraud spreads through shared
directors: are fraudulent healthcare companies connected to each other
through the people who run them?

## Approach
- Data: Dutch Trade Register records of companies and directors,
  linked to municipal fraud registrations.
- Method: a company-director network, projected to a
  company-to-company network; community detection and a comparison of
  fraudulent and non-fraudulent companies before and after 2015.
- Tools: R (igraph, tnet, netseg).

## What I found
- Healthcare companies that share a director with a fraudulent company
  are often fraudulent themselves.
- Against expectations, the connections between fraudulent companies
  and their directors did not differ significantly before and after 2015.
- Recommendation: municipalities can use network analysis of shared
  directors to flag companies that deserve a closer look.

## Files
- `compleet network.R`: the full network
- `hypothese 1 t-test.R`: hypothesis 1, before and after 2015
- `hypothese_2.R`: hypothesis 2, directors with more than one company
- `syntax.R`: network figures
- `dump.R`: scratch file with experiments; does not run as is

## Notes
- The data is not included. It contains company, personal and fraud
  records that cannot be shared, so the scripts do not run on their own.
- The code is kept exactly as I wrote it in 2021, as a student and
  before AI coding tools existed.
