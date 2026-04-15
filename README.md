# Partial Association Explorer

**Partial Association Explorer** is an interactive **Shiny** application for exploring associations between variables while **controlling for one or more additional variables**.  
It provides a unified framework for partial association measures across different variable types:

| Variable Types | Partial Association Measure | Method |
|---------------|----------------------------|--------|
| Categorical vs. Categorical | **Partial Cramer's V** | Multinomial likelihood approach (with fallback to regular Cramer's V) |
| Continuous vs. Continuous | **Partial Correlation (Partial r)** | Added-variable regression |
| Continuous vs. Categorical | **Partial η² (Effect Size)** | ANCOVA-based partitioning |

The app also includes:
- A **correlation/association network visualization** (using `visNetwork`)
- **Interactive contingency tables** and summaries
- **Scatter plots and added-variable plots** for continuous associations
- Optional **threshold filtering** to focus on strong associations
- Dataset **uploading and variable selection** interface

---

## Features

### ✅ Upload & Manage Data
- Import `.csv`, `.xlsx`, or `.xls` files
- Automatically detects variable types
- Clean variable names via `{janitor}`

### 🎯 Association Analysis
- Computes **partial and regular** association measures depending on variable types
- Allows selection of **control variables** to isolate relationships
- Displays effect size **strength indicators** and test statistics (χ², F, etc.)

### 🔗 Network Visualization
- Builds a network graph of associations between variables
- Edge width reflects **strength of association**
- Adjustable thresholds to declutter the network
- Interactive hover and click behavior

### 📊 Plots and Tables
- **Scatter plots + Added-variable plots** for continuous variables
- **Contingency tables** with dynamic formatting
- **Distribution summaries** for selected variables

---

## Contributing

Contributions are welcome. Please read [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on reporting bugs, suggesting enhancements, and submitting pull requests.

This project follows the [Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md).

## License

This project is licensed under the [MIT License](LICENSE).
