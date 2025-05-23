# Academic Articles Repository

This repository contains research articles, code, and data for various economic studies.

## 📁 Repository Structure

### 2025_01_household_income/
**"Growing but Not Prospering: Armenia's Paradox of Economic Growth and Household Welfare"**

- **Status**: Submitted to Baltic Journal of Economics
- **Period Analyzed**: 2022-2023 Armenian household income dynamics
- **Dataset**: 121,170 household observations from Armenia's Integrated Living Conditions Survey

#### Key Files:
- `article_20250522.rmd` - Main R Markdown manuscript
- `article_20250522.docx` - Compiled Word document  
- `article_20250522.pdf` - PDF version
- `bib.bibtex` - Bibliography file
- `files/household_income_db.csv` - Processed household data
- `files/initial_modeling.R` - Statistical analysis code

#### Research Highlights:
- **The Paradox**: Despite 12.6% GDP growth in 2022, real household incomes declined due to 8.3% inflation
- **The Resolution**: 2023's moderate growth (7.2%) with price stability (-0.6% inflation) generated broad-based welfare improvements
- **Key Finding**: Price stability serves as critical mediating factor between economic growth and household welfare

#### Methodology:
- Two-stage analytical approach combining regional fixed effects with inflation adjustments
- Weighted log-linear regression with regional and settlement interactions
- Natural experiment design leveraging Armenia's contrasting economic conditions (2022 vs 2023)

### Other Projects:
- `2024_09_armenia_power/` - Armenia's electricity sector analysis
- `2024_09_russia_oil_exports/` - Russian oil export dynamics

## 📊 Data Sources

- **Armenia Statistical Committee**: Integrated Living Conditions Survey
- **Central Bank of Armenia**: Monthly CPI data
- **World Bank**: Macroeconomic indicators

## 🔧 Technical Requirements

- R 4.0+
- Required packages: `tidyverse`, `knitr`, `kableExtra`, `ggridges`, `Hmisc`
- LaTeX distribution (for PDF compilation)

## 📖 Citation

```bibtex
@article{tavadyan2025household,
  title={Growing but Not Prospering: Armenia's Paradox of Economic Growth and Household Welfare},
  author={Tavadyan, Aghasi},
  journal={Baltic Journal of Economics},
  year={2025},
  status={Under Review}
}
```

## 👤 Author

**Aghasi Tavadyan**  
Associate Professor, Armenian State University of Economics  
Founder, Tvyal.com (Data Science Organization)  
Email: aghasi.tavadyan@asue.am  
ORCID: https://orcid.org/0000-0002-1644-6205

## 📄 License

This research is available for academic use. Please cite appropriately if using any materials.

---

*Last Updated: May 2025*