

use "Lobbyist Survey.dta"

* Figure 2
histogram who_determine, discrete percent ylabel(0(20)60, angle(horizontal)) fcolor(green) lcolor(green) gap(20) xsc(r(1 4)) xtitle("") xlabel(1 "Lobbyist"  2 "Principal"   3 `" "Joint" "decision" "') title("Who decided whether or not to take a position on this bill?") graphregion(fcolor(white) ifcolor(white)) text(17.6  1 "15.6") text(29.8  2 "27.8") text(58.7  3 "56.7")
graph export "Figure 2.eps", as(eps) preview(off)

* Figure 3
histogram why_determine, discrete percent  ylabel(0(20)60, angle(horizontal)) fcolor(green) lcolor(green) gap(20)  xtitle("") xlabel(1 "Stakes"  2 `" "Likelihood"  "of outcome" "'   3 `" "Both" "factors" "' 4 "Other") title("What was the most important determinant in" "deciding on whether to lobby on the bill? ") graphregion(fcolor(white) ifcolor(white))  text(55.0  1 "53.0") text(5.1  2 "3.1")text(30.3  3 "28.3") text(17.6  4 "15.6")
graph export "Figure 3.eps", as(eps) preview(off)

* Figure 4
histogram certainty_outcome, discrete percent ylabel(0(20)60, angle(horizontal)) fcolor(green) lcolor(green) gap(20) xtitle("") xlabel(1 `" "Very" "certain pass" "' 2`" "Somewhat" "certain pass" "' 3 "Uncertain"  4 `" "Somewhat" "certain fail" "' 5 `" "Very" "certain fail" "') title("When you took the position on the bill, how certain" "were you that the bill would be enacted into law?") graphregion(fcolor(white) ifcolor(white)) text(9.2  1 "7.2") text(30.1  2 "28.1") text(53.3  3 "51.3") text(13.2  4 "11.2") text(4.3  5 "2.3")
graph export "Figure 4.eps", as(eps) preview(off)


**Table SI.3
tab why_determine certainty_outcome, cell chi2
