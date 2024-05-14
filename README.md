# Mortality Analysis of Chilean Annuitants

## Project Overview
This project undertakes a detailed analysis of mortality data of annuitants in Chile, with the goal of informing decision-making for annuity pricing in the Chilean insurance market. The analysis includes descriptive statistics, survival analysis, life table graduation, and an ethical evaluation of gender-based pricing.

## Data Analysis Stages

### Exploratory Data Analysis (EDA)
- **Objective**: To explore the dataset containing 1,292,017 annuitants aged 60 or more, identifying initial patterns and anomalies.
- **Outcome 1**: Identification of the distribution of key variables such as age, sex, health status, and person type.
- **Outcome 2**: Identification of potential biases and insights into the distribution of deaths across different age groups and covariates.

### Survival Analysis
- **Approach**: Application of both semi-parametric and non-parametric techniques, including Cox regression and Kaplan-Meier (KM) estimations.
- **Findings**:
  - Survival probability decreases with age, with significant differences observed based on sex, health status, and person type.
  - Male annuitants and disabled annuitants have higher mortality rates compared to their female and healthy counterparts, respectively.
  - Interaction effects were found between sex and person type, indicating differential mortality patterns within subgroups.
- **Challenges**: Violation of the proportional hazards assumption in Cox regression, necessitating reliance on KM estimations.

### Graduation of Life Table
- **Objective**: To construct a unisex life table for healthy annuitants aged 60 to 99.
- **Methodology**:
  - Calculation of crude mortality estimates.
  - Fitting of graduation models, including Gompertz, Makeham, and cubic splines.
  - Selection of the best model based on qualitative and quantitative criteria.
- **Recommended Model**: A regression spline with knots at ages 72.5 and 87.5.
- **Key Results**: The unisex life table shows mortality probabilities that are intermediate between those of male and female annuitants.

### Ethical Considerations
- **Focus**: Analysis of the ethical implications of using gender as a rating factor for annuity pricing.
- **Pros**: Gender-based pricing reflects actuarial fairness by aligning premiums with risk.
- **Cons**: It may reinforce social inequalities and unfairly penalize individuals based on factors they cannot control.
- **Recommendation**: Replace gender with direct risk factors, such as health behaviors, to ensure fairness while maintaining actuarial soundness.

## Key Achievements

### Academic Recognition
- **High Praise**: The project received commendation for its comprehensive analysis and contribution to the field of actuarial science.
- **Grade Awarded**: A high grade reflecting the project's excellence and thoroughness.

### Project Ranking
- **Top 5%**: The project was ranked in the top 5% of all submissions, distinguished by its accuracy and comprehensive approach.

## Key Findings
- **Significant Predictors**: Factors such as age, sex, health status, and person type significantly influence mortality rates.
- **Unisex Life Table**: The recommended unisex life table provides a balanced approach to mortality estimation, avoiding gender bias while maintaining accuracy.

## Methodology
- Comprehensive methodologies were documented for each stage of the project, from data cleaning and preparation to statistical analysis and model evaluation.
- Overcoming challenges such as data imbalance and model selection through rigorous testing and performance tuning.

## Conclusion and Implications
- The findings underscore the critical role of data-driven analysis in identifying risk factors and informing road safety strategies.
- The project’s data-driven approach provides a foundation for informed decision-making and policy development aimed at enhancing road safety.

## Technical Details

### Tools Used
- **R**: The analysis was primarily conducted using R, leveraging its statistical and data analysis capabilities.

### Challenges Overcome
- **Data Imbalance**: Addressed through robust data cleaning and preprocessing.
- **Model Selection**: Optimized model performance to enhance predictive accuracy.

## Contact
For further information or inquiries, please contact Dhwanish Kshatriya at [your-email@example.com].
