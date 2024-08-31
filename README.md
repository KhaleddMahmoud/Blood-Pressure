# Blood Pressure Reduction Study

## Introduction

### Background
High blood pressure (hypertension) significantly increases the risk of heart disease, stroke, and other serious health issues. This study evaluates the efficacy of an arginine-containing drug in reducing systolic blood pressure at different dosages.

### Objective and Aim
The study aims to:
1. Determine the effectiveness of the drug at doses of 0, 2, 5, and 10 mg/day.
2. Identify outliers in the blood pressure reduction data.
3. Examine the correlation between drug dosage and blood pressure reduction.
4. Assess statistical properties such as normality and homoscedasticity.
5. Test hypotheses regarding drug effectiveness and dose differences.
6. Fit and interpret a linear regression model to the data.

## Methodology

### Data Reading
The dataset was imported, examined, and ensured for integrity and comprehensiveness.

### Descriptive Statistics
- **Mean**: 7.625 mmHg
- **Median**: 6.5 mmHg
- **Minimum**: -8 mmHg
- **Maximum**: 26 mmHg
- **Quartiles**: 25th percentile = 2.75 mmHg, 75th percentile = 13.00 mmHg

### Graphics
- Bar chart of gender distribution
- Bar chart of mean blood pressure reduction by gender
- Histograms of dose and blood pressure reduction
- Scatterplot of dose vs. blood pressure reduction with regression lines by gender
- Boxplots of blood pressure reduction by dose

### Outlier Detection
No outliers were detected in the blood pressure reduction data.

### Testing for Normality and Homoscedasticity
- **Normality**: Shapiro-Wilk test and Q-Q plot indicated normal distribution.
- **Homoscedasticity**: Bartlett's and Levene's tests confirmed equal variances across dose levels.

### Statistical Inference
- Confidence intervals at 90%, 95%, and 99% levels for mean blood pressure reduction.

### Hypothesis Testing
1. **Control Group Comparison**: Significant difference between males and females in the control group.
2. **High Dose vs. Control**: Significant difference in blood pressure reduction between 10 mg and 0 mg doses.
3. **Comparison Between Doses**: ANOVA indicated significant differences across various doses with post-hoc tests confirming specific pairwise differences.

### Linear Model
1. **Gender Effect**: No significant relationship between gender and blood pressure reduction.
2. **Dose Comparison (High Dose vs. Control)**: Significant relationship with 78.64% of variation explained.
3. **Dose Across All Levels**: Significant relationship with 73.91% of variation explained.

## Conclusion
The study demonstrates that higher doses of the arginine-containing drug are more effective in reducing blood pressure. Gender differences in response were noted, with males generally experiencing greater reductions. The results suggest that the drug is effective, particularly at higher doses, with implications for hypertension management. Further research on gender-specific responses and optimal dosing is recommended.

