# Academic Performance Analytics System
### Automated Data Pipeline for Educational Decision Support

*Production analytics solution transforming manual, discussion-based reporting processes into reproducible, data-driven insights for academic leadership.*

---

## 🎯 Project Overview

This system was developed for the **School of Medical Sciences (SMS), The University of Auckland**, to replace fragmented, discussion-based reporting with a **fully automated, statistically rigorous analytics workflow**. It processes **10,776 anonymised student records** across **23 undergraduate** and **37 postgraduate MEDSCI papers** (2020–2024), enabling reproducible semester- and multi-year analyses.

### Business Challenge:
- **Manual discussion processes** consuming 20+ hours per Board of Examiners meeting
- Academic leaders making decisions based on **individual spreadsheet summaries** rather than integrated statistical evidence (some departments relying entirely on verbal discussions during the meeting)
- **No standardised analytics framework** for semester-based performance review across multiple papers and student cohorts
- **Reactive rather than predictive** approach to student support and curriculum planning

---

## 💡 Key Analytical Modules & Features

### 1. **Automated Data Integration Pipeline**
- **Multi-source ETL:** Consolidates grades from individual paper CSV files into unified analytical dataset
- **Data Quality Assurance:** Automated validation, test student filtering, and anonymisation protocols
- **Reproducible Workflows:** Version-controlled analysis with comprehensive documentation

### 2. **Cohort Construction (Unsupervised Learning)**
- **Hierarchical clustering** with Gower's distance on paper enrollment patterns
- Enables **cohort-adjusted performance comparisons** across different academic pathways
- Supports targeted intervention strategies for distinct student groups

### 3. **Statistical Performance Analysis**
- **Intra- & Inter-Cohort Comparisons:** Standardisation by year and paper for fair cross-group analysis
- **ANOVA, Kruskal–Wallis, and pairwise tests** to detect grading discrepancies across papers
- **Advanced modelling:** Classical and Bayesian regression analysis for trend identification

### 4. **COVID-19 Impact Assessment**
- **Comparative analysis** of 2020–2022 vs 2023–2024 performance using bootstrapping and Bayesian Student-t models
- **Key finding:** Undergraduate grades dropped significantly post-COVID (0.65–1.83 points on average)
- Accounts for online learning transitions across program levels

### 5. **Predictive Academic Success modelling**
- **Hierarchical regression models** identifying Stage II papers (MEDSCI 204–206) as strongest predictors of Stage III outcomes
- **Interaction tests** for cohort-specific effects and early warning indicators
- **Variance explanation:** Up to **69% of Stage III outcomes** predicted by early-stage performance

### 6. **Performance Deviation Detection**
- **Algorithmic identification** of "overperformers" who improve markedly in later stages
- Supports **targeted academic support** and curriculum optimisation strategies

---

## 📊 Key Insights & Business Impact

### Selected Analytical Findings:
- **Grading consistency issues:** Certain Stage III capstone projects (PHYSIOL 399, PHARMCOL 399) consistently graded higher than comparable papers
- **Foundational course analysis:** Stage II MEDSCI 201 showed consistently lower performance compared to 205/206 across cohorts
- **COVID impact variation:** Mixed effects across program levels, with targeted patterns by paper type
- **Predictive modelling success:** Early academic performance explains significant variance in later outcomes

### Measurable Business Impact:
- ✅ **70% reduction** in Board of Examiners meeting preparation time
- ✅ **standardised analytics framework** replacing fragmented spreadsheets and verbal discussions
- ✅ **Evidence-based decision-making** supporting 10 academic heads across the school
- ✅ **Scalable architecture** expanded from departmental tool to school-wide resource
- ✅ **Reproducible workflows** enabling consistent semester-to-semester analysis

---

## 🛠️ Technical Stack

**Core Technologies:**
- **R Programming:** Advanced statistical modelling and data manipulation
- **Data Engineering:** `dplyr`, `tidyr` for robust ETL processes
- **Statistical modelling:** `brms` (Bayesian), `boot` (bootstrapping), base R regression
- **Machine Learning:** `cluster`, `factoextra` for unsupervised learning
- **visualisation:** `ggplot2`, `bayesplot` for publication-quality graphics
- **Documentation:** RMarkdown for reproducible reporting
- **Version Control:** Git for collaborative development

**Architecture Principles:**
- **Modular design** enabling easy maintenance and expansion
- **Automated validation** and quality assurance workflows
- **Stakeholder-centric outputs** designed for non-technical academic users
- **Privacy-first approach** with comprehensive anonymisation protocols

---

## 📂 Repository Structure

```
grade_analysis/
├── README.md                    # Project overview and documentation
├── src/                         # Core R scripts and functions
│   ├── data_processing.R        # ETL pipeline and data integration
│   ├── statistical_analysis.R   # Advanced modelling and testing
│   └── visualisation.R          # Plotting and report generation
├── data/                        # anonymised sample datasets
├── reports/                     # Example analytical outputs (PDF, HTML)
├── figures/                     # Generated plots and charts
└── requirements.txt             # Package dependencies
```

---

## 🚀 Professional Development Outcomes

This project demonstrates **entrepreneurial thinking within organisational constraints**, showcasing ability to:

- **Identify process inefficiencies** in traditional academic workflows
- **Lead technical solution development** from concept to production deployment
- **Manage complex stakeholder requirements** across multiple academic departments
- **Deliver measurable business value** through automated analytics
- **Bridge technical-business communication** gaps effectively

**The system exemplifies my approach to identifying data opportunities within existing organisational processes - the same innovative mindset I would apply to optimising operational analytics, customer insights, and digital experience platforms in dynamic business environments.**

---

## 🔮 Future Enhancement Opportunities

**Potential Technology Integrations:**
- **Interactive Dashboards:** Power BI/Tableau integration for real-time visualisation
- **Cloud Deployment:** Azure/AWS integration for enhanced scalability and accessibility
- **API Development:** Real-time data ingestion and integration with university systems
- **Extended Predictive modelling:** Machine learning for enrollment planning and resource optimisation

---

## 📈 Connection to Broader Analytics Applications

The methodologies and system architecture developed here are **directly applicable to diverse industry contexts**, including:

- **Customer Analytics:** Cohort analysis and performance segmentation
- **Operational Intelligence:** Process optimisation and efficiency measurement
- **Predictive modelling:** Risk assessment and early warning systems
- **Stakeholder Reporting:** Executive dashboard development and automated insights
- **Data Pipeline Engineering:** Scalable ETL architecture for large, complex datasets

*This project exemplifies my approach to identifying data opportunities within existing organisational processes - the same innovative mindset I would apply to optimizing operational analytics, customer insights, and digital experience platforms in dynamic business environments.*
