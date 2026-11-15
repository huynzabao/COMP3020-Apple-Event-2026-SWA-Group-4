# MEMBER A | Q1 + Q4 | REPORT DRAFT (PROVISIONAL)

Dataset run: `APPLE_EVENT_2026_RUN_20261028_045034`.
Manual event overrides (from audit): 200.
All interpretations must be refreshed AFTER the two team members validate labels and rerun cleaning.

## Q1(a). Subtopic definition

The subtopic is explicit mention of 'iPhone Duo' in an English-declared post identified as relevant to the September 2026 Apple Event. Each URI is one post. Keyword rules were applied consistently; sampled human checks are required to assess misclassification. This does NOT measure positive/negative sentiment.

## Q1(b). Pearson chi-square

H0: Before/After period and iPhone Duo mention status are independent in the retrieved sample.
H1: Period and iPhone Duo mention status are associated.
Before: Yes 84, No 384 (share 17.95%).
After: Yes 2552, No 1516 (share 62.73%).
Pearson chi-square = 345.8297, df = 1, p = <1e-16, Cramer's V = 0.2761.
Interpret the result as an association in retrieved posts, NOT as causal proof or a platform-wide prevalence estimate.

## Q1(c). Twitter's chi-square trend score

Module 9's simplified Twitter chi-square treats past counts as expected; its standard displayed formula assumes equal N. Because retrieved N differs across periods, expected After counts were scaled by the Before proportion.
E_After,Yes = 730.154; E_After,No = 3337.846.
Twitter chi-square score = 5540.177.
It differs from Pearson's independence statistic because Pearson estimates expected cells from pooled row/column margins, while Twitter's simplified score treats the Before proportion as fixed. The latter ignores uncertainty in the Before estimate; it is only a descriptive trend score here.

## Q1(d). Daily simple linear regression

Fitted line: retrieved_posts = 152.229 + (26.426) * day_index.
Slope = 26.426 retrieved posts per successive 24h bin; R2 = 0.0347.
Peak retrieved daily count = 2308 at 24h bin 7.
A positive/negative slope indicates the fitted direction across these 14 sample bins; inspect the plot before claiming that the pattern is adequately linear.

## Q1(e). Limitations

Viral discussion may spike near launch and decay, violating linearity. Daily counts are nonnegative, serially dependent and unevenly sampled; a single straight-line model may obscure the event-day peak. Search queries, 15 potentially truncated search leaves from the prior collection audit, English metadata and pre/post selection restrict generalisability. 'iPhone Duo' being a search keyword can also inflate subtopic coverage.

## Q4(a). Most active topic authors

Top authors by number of currently included topic posts: technology-news.bsky.social (85); appleinsider.com (75); feed.igeek.gamer-geek-news.com.ap.brid.gy (70).
This ranking is by posts, NOT by follower counts.

## Q4(b-c). Follow-network and centrality

Network status: REUSE_OK.
A directed edge (u -> v) means account u follows account v. For each of the three selected roots, the API snapshot requested up to 50 followers and 50 followed accounts (if available). This is an observed partial, time-of-retrieval network, not necessarily the follow network at the September event.
Graph contains 153 vertices and 162 directed edges. In-degree counts observed incoming follows; out-degree counts observed outgoing follows. PageRank uses damping 0.85.
Top in-degree DID: did:plc:nxmjxl2wiyat43p3gikkt37w; top out-degree DID: did:plc:m2yuqynl2cttvi4k5453yegh; top PageRank DID: did:plc:nxmjxl2wiyat43p3gikkt37w.
Degree and PageRank can rank nodes differently: degree counts observed links locally, whereas PageRank weights inbound links by the importance and link distribution of the accounts providing them. Restricted graph sampling and unknown account handles limit interpretation.

## To finish before submitting

1. Complete manual validation of event and subtopic labels and rerun `COMP3020_02_CLEAN_AND_TEAM_HANDOFF.R`.
2. Rerun this Member A script and inspect all CSV + PNG results.
3. If top three active accounts changed, collect their network again via public API, update the handoff edge file and re-run Q4.
4. Use only claims directly supported by the updated output; cite the COMP3020 Modules and describe sample limitations.
