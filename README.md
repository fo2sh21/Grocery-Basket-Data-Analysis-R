# 🛠 Data Analysis Dashboard (R + Shiny)

An **interactive data analysis dashboard** built with **R** and **Shiny**, designed to upload, clean, analyze, visualize, and explore transactional datasets.  
The app combines **data cleaning**, **exploratory analysis**, **customer clustering**, and **association rule mining** into one intuitive interface.

---

## 📌 Features

### 1️⃣ Data Upload & Cleaning
- Upload CSV datasets.
- Automatic:
  - Duplicate removal.
  - NA value removal.
  - Outlier detection & removal (boxplot method).
- Data structure inspection.

### 2️⃣ Data Visualization
- **Total spending by payment type** (Bar chart).
- **Spending by age** (Line chart).
- **Spending by city** (Bar chart with custom color palette).
- **Spending distribution** (Frequency polygon).

### 3️⃣ Customer Insights
- Extracts **unique customer profiles**:
  - Name, age, cities visited.
  - Total payments by method.
  - Preferred payment method (with % share).
- Transaction history per customer.
- Pie chart of payment method usage.

### 4️⃣ Clustering Analysis
- **K-Means clustering** of customers by:
  - Age.
  - Total spending.
- Adjustable cluster count (2–4).
- Interactive cluster table & labeled scatter plot.

### 5️⃣ Association Rules (Market Basket Analysis)
- Generates **Apriori rules** from transaction data.
- Adjustable **support** and **confidence** thresholds.
- Outputs sorted rules by confidence & support.

### 6️⃣ List Views & Quick Filtering
- Explore:
  - Unique items.
  - Cities.
  - Payment types.
- Click to filter and view related transactions.

---

## 🖥️ Tech Stack

| Component       | Technology |
|----------------|------------|
| **Frontend UI** | Shiny, bslib (Bootstrap themes), DT (interactive tables) |
| **Backend**     | R |
| **Data Viz**    | ggplot2, ggrepel, scales |
| **ML**          | K-Means Clustering |
| **Association Rules** | arules |
| **Data Handling** | dplyr, tidyr, readr |

---

## 📦 Installation

### Prerequisites
- R (≥ 4.0 recommended)
- RStudio (optional, but preferred)

### Required Packages
Install all dependencies in one go:
```R
install.packages(c(
  "dplyr", "scales", "arules", "ggplot2", "tidyr", "readr", 
  "shiny", "DT", "bslib", "ggrepel", "RColorBrewer"
))
```

---

## 🚀 Running the App

1. Clone the repository:
\`\`\`bash
git clone https://github.com/yourusername/data-analysis-dashboard.git
cd data-analysis-dashboard
\`\`\`

2. Open the project in RStudio (or run in terminal).

3. Run the app:
\`\`\`R
shiny::runApp("FINAL_VERSION.R")
\`\`\`

4. Open the link in your browser (Shiny will print it, usually \`http://127.0.0.1:xxxx\`).

---

## 📂 File Structure

\`\`\`
📁 data-analysis-dashboard
│── FINAL_VERSION.R                # Main Shiny app (UI + server)
│── README.md            # Project documentation
│── items.txt            # Generated transactions file (when running AR)
│── grc.csv      #given dataset
\`\`\`

---


## 📊 Example Screenshots

Insert the data and let the program process and clean it
> <img width="605" height="212" alt="Screenshot 2024-12-18 105521" src="https://github.com/user-attachments/assets/53a4ccf3-9bd9-4f0d-8125-ee89eca5286e" />
Customer Transactions should be visible
> <img width="1251" height="683" alt="customers 2" src="https://github.com/user-attachments/assets/b5ca3ae7-57f2-42c8-9066-50c22e197494" />
> <img width="1897" height="927" alt="lists cities" src="https://github.com/user-attachments/assets/79db7f59-dd82-4cd0-98ea-ef12dff01861" />
> <img width="1919" height="713" alt="lists items 1" src="https://github.com/user-attachments/assets/3f1ef2ef-c185-4d0b-be85-a23be353a5cd" />
> <img width="1574" height="606" alt="lists items 2" src="https://github.com/user-attachments/assets/45af3b2b-7550-49de-a9ed-b5809f8e0eec" />
> <img width="1903" height="964" alt="lists payment types" src="https://github.com/user-attachments/assets/98c607a8-ec08-4759-a49c-d1f33bb6202e" />
Visualizations for plots about the customers
> <img width="1919" height="699" alt="age" src="https://github.com/user-attachments/assets/78ff1f4a-8d51-4ecf-b24a-eee2f79ab929" />
> <img width="1918" height="692" alt="city" src="https://github.com/user-attachments/assets/9718e920-572f-4872-9c2e-89afb5109e58" />
> <img width="1917" height="538" alt="distribution" src="https://github.com/user-attachments/assets/b074f533-434c-4bf6-a926-4454d26d35e0" />

K-means clustering of customers
> <img width="1890" height="914" alt="Clusters1" src="https://github.com/user-attachments/assets/65cf26b4-1303-4de5-8c5e-8b467c720a22" />
> <img width="1268" height="636" alt="clusters2" src="https://github.com/user-attachments/assets/ab30aa70-dcd6-4de8-b5a2-54ede01a9874" />
Associaction rules with modifiable parameters to get the optimal result
> <img width="1918" height="763" alt="Screenshot 2024-12-18 112049" src="https://github.com/user-attachments/assets/0e9d87b4-a93f-4cf5-8eee-f948dff4d87d" />
Dynamic dashboard for each individual customer
> <img width="1900" height="543" alt="customers 1" src="https://github.com/user-attachments/assets/cd126e4a-8325-409a-a1ce-653dddbe4a78" />


## 📜 License
MIT License — Free to use and modify, with attribution.
