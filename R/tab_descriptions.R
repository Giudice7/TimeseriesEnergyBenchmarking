tab_descriptions <- list(
  load_data = list(title = "Load data", content = withMathJax(HTML(
    paste0(
      "<h3>📥 Welcome to the Energy Benchmarking Tool</h3>",

      "<p>This application is designed to help you analyze and benchmark your building’s electrical energy consumption using hourly time series data.
    It evaluates your building’s performance by calculating a set of <strong>Key Performance Indicators (KPIs)</strong> and compares them to similar buildings (your peers) in the same Primary Space Usage (PSU) category, such as Office or Education buildings.</p>",

      "<p>The benchmarking process involves identifying your peer group based on features of your electric load — including thermal sensitivity, load shape, and load conditions (e.g., weekdays vs. weekends, winter vs. summer).
    The KPIs are then converted into <strong>Performance Scores</strong> on a 0–100 scale, where higher scores indicate better performance.</p>",

      "<p>You can use the application in two ways:</p>",
      "<ul>
      <li><strong>Inspect mode:</strong> Toggle the switch at the top to explore the app using default example buildings. This is helpful for understanding how the tool works without uploading your own data.</li>
      <li><strong>Upload mode:</strong> Upload your building’s data to perform a full benchmarking analysis. You’ll need:
        <ul>
          <li>A CSV file with your building’s hourly energy consumption, containing:
            <ul>
              <li><code>timestamp</code> column in the format <code>%Y-%m-%d %H:%M:%S</code></li>
              <li><code>power</code> column with electric consumption in kW</li>
            </ul>
          </li>
          <li>A CSV file with hourly outdoor air temperature, containing:
            <ul>
              <li><code>timestamp</code> column in the same format</li>
              <li><code>airTemperature</code> column with values in °C</li>
            </ul>
          </li>
          <li>The gross floor area of your building in m²</li>
          <li>The building’s Primary Space Usage (PSU) category (Office or Education)</li>
          <li><em>Optional:</em> The state or location of the building to exclude local holidays from analysis</li>
        </ul>
      </li>
    </ul>",

      "<p>After all data is provided, click <strong>Perform the analysis</strong> to start the evaluation and explore your results in the other tabs.</p>"
    )
  ))),

  pre_processing = list(title = "Pre-processing", content = withMathJax(HTML(
    paste0(
      "<p>Data pre-processing aims to clean the energy consumption time series from outliers, missing values, and prolonged constant values.
        The <strong>Multi-Seasonal and Trend decomposition using LOESS (MSTL)</strong> technique is employed to extract trend and seasonality before identifying anomalies.
        The cleaning process includes the following steps:</p>",

      "<ul>",
      "<li><strong>Extreme values filtering:</strong> Very high and low values are flagged using thresholds:
          <ul>
            <li>High threshold (HT): $$ HT = pct_{95} \\cdot 2 $$</li>
            <li>Low threshold (LT): $$ LT = pct_{5} / 2 $$</li>
          </ul>
          Values beyond these thresholds are marked as missing.</li>",

      "<li><strong>Outlier detection using MSTL:</strong> The time series is decomposed into:
          <em>Trend, Seasonal24 (daily), Seasonal168 (weekly), and Remainder</em> components.
          Outliers are identified in the Remainder using the IQR method:
          $$ OUT = Q_1 - 5 \\cdot IQR \\lor Q_3 + 5 \\cdot IQR $$
          Detected outliers are set to NaN.</li>",

      "<li><strong>Flat segments:</strong> Continuous constant values lasting more than 6 time steps are considered invalid and replaced with NaN.</li>",

      "<li><strong>Imputation:</strong> All NaN values are filled using a look-up table that stores the mean energy consumption for each hour of the day and load condition.</li>",
      "</ul>"
    )
  ))),

  peer_identification = list(title = "Peer Identification", content = withMathJax(HTML(
    paste0(
      "<p> The peer identification process is a step performed to identify a group of similar buildings against which compare your building. </p>",
      "<p> This identification process is performed taking into account several aspects,
      such as: </p>",
      "<ul>",
      "<li> The end-use category (or Primary Space Usage PSU) of the building: when a new building is benchmarked, its peers are searched among buildings with the same end-use category. </li>",
      "<li> Reference load conditions among the yearly electrical energy consumption time series: when a new building is benchmarked its yearly energy consumption time series is firstly chunked into sub-sequences of daily length and all the obtained daily load profiles are then grouped in so-called ”Load conditions” a-priori identified with a domain-expert approach (i.e., winter workdays, winter weekends and holidays, summer workdays, summer weekends and holidays).
      In particular, non-working days are identified as days with a low standard deviation of the energy consumption.</li>",
      "<li>  The sensitiveness of the electric load to the external air temperature: when a new building is benchmarked, for some of the pre-identified load conditions (i.e., Winter workdays, Summer workdays) the electrical load is classified, by means of a statistical correlation analysis, as thermal sensitive or not.
      In particular, the Spearman's rank correlation coefficient is used to identify a potential correlation, and for those with a Sperman > 0.4, a regression analysis is performed. </li>",
      "<li> The mean daily energy consumption: when a new building is benchmarked, for some of the pre-identified load conditions (i.e., Winter workdays, Summer workdays) is calculated the mean daily energy consumption. </li>",
      "<li> A load shape factor F : when a new building is benchmarked, for some of the pre-identified load conditions (i.e., Winter workdays, Summer workdays) is calculated an indicator that is representative of the shape of daily load profiles. </li>",
      "</ul>",
      "<p> In particular, once the load condition of the building are identified, the thermal correlation analysis is performed, which can return 'Thermal sensitive' or 'Non-thermal sensitive' for the two load conditions 'Winter workdays' and 'Summer workdays'.
      Subsequently, the identification of the most similar peers to the building under analysis is performed considering two different metrics: the mean daily energy consumption and the load shape factor F, calculated as: ",
      withMathJax("\\(F = \\frac{E_{night}\\:[kWh]}{E_{day}\\:[kWh]}\\)"),
      ", where ",
      withMathJax("\\(E_{night}\\)"),
      " is the amount of energy consumption in the interval [20:00-07:00] and ",
      withMathJax("\\(E_{day}\\)"),
      " is the amount of energy consumption in the interval [08:00-19:00] separately evaluated for all the days in the load conditions 'Winter Workday' and 'Summer Workday'.
      Finally, the 30 nearest neighbors are extracted from the dataset using the Euclidean distance as a similarity measure in the geometrical space. In particular, the calculated Euclidean distance is differently weighted among the two metrics, giving the 70% of the importance to the peer similarity on E and the remaining 30% of the importance to the peer similarity on the load shape factor F. </p>"
    )
  ))),

  kpi = list(title = "Key Performance Indicators (KPIs)", content = withMathJax(HTML(
    paste0(
      "<p>This section provides insights into the performance of the building through a series of KPIs derived from its energy consumption time series. The KPIs are grouped into the following categories:</p>",

      "<ul>",

      "<li><strong>Energy Use Intensity (EUI):</strong> Represents the energy consumed per square meter. If the load condition is identified as <em>thermal-sensitive</em>, the EUI is further normalized using degree days to ensure a fair benchmarking process.</li>",

      "<li><strong>Operation Schedules:</strong> Focuses on energy use during <em>ON-hours</em> (high-usage periods), <em>OFF-hours</em> (low-usage periods), and weekends. Two KPIs are defined:
        <ul>
          <li><strong>OFF-impact:</strong><br/>
          \\( \\text{OFF-impact} = \\frac{E_{\\text{OFF-hours}} - E_{\\text{ON-hours}}}{E_{\\text{ON-hours}}} \\cdot 100 \\)</li>
          <li><strong>Weekend-impact:</strong><br/>
          \\( \\text{Weekend-impact} = \\frac{E_{\\text{weekends}} - E_{\\text{ON-hours}}}{E_{\\text{ON-hours}}} \\cdot 100 \\)</li>
        </ul>
      </li>",

      "<li><strong>Load Volatility:</strong> Assesses the variability of daily load profiles by measuring their average distance from their 10% nearest neighbors, normalized by energy use. For thermal-sensitive conditions, neighbors are based on similar outdoor temperature profiles:<br/>
      \\( LV_i = \\frac{\\text{mean}(\\vec{d_i})}{E_i} \\cdot 100 \\)</li>",

      "<li><strong>Anomalous Energy Consumption:</strong> Calculates the proportion of daily load profiles considered anomalous using three statistical methods (IQR, Z-score, and MAD):<br/>
      \\( AR = \\frac{\\# \\text{anomalies}}{N} \\cdot 100 \\)</li>",

      "<li><strong>Frequency of Load Patterns:</strong> Analyzes how often typical load shapes occur in the dataset. Load shapes are extracted through clustering and labeled as frequent or infrequent based on occurrence in the overall end-use category shapes.</li>",

      "</ul>"
    )
  ))),

  benchmarking = list(title = "Benchmarking", content = withMathJax(HTML(
    paste0(
      "<p>Once the KPIs are calculated for a new building, they are benchmarked by comparing them with the statistical distributions of the same KPIs from the identified peer group and the broader end-use category.</p>",

      "<p>To standardize the results, each KPI is transformed into a <strong>Performance Score (PS)</strong> ranging from 0 (low performance) to 100 (excellent performance). The transformation is performed using the formula:</p>",

      "<p style='text-align:center;'>\\( \\text{Performance score (KPI}_i) = 100 - \\text{pct}(\\text{KPI}_i) \\)</p>",

      "<p>where \\( \\text{pct}(\\text{KPI}_i) \\) is the percentile rank of the KPI value among the peer distribution. If the percentile is already aligned with the 0–100 convention, it is used directly as the performance score.</p>"

    )
  ))),

  summary = list(title = "Result Summary", content = HTML(
    paste0(
      "<p>The <strong>Result Summary</strong> section provides a consolidated overview of the benchmarking outcomes for the building under analysis.</p>",

      "<p>It summarizes the computed families of KPIs and their corresponding <strong>Performance Scores (PS)</strong>, which are derived by evaluating where the building's KPIs fall within the frequency distributions of its peer group.</p>",

      "<p>These scores reflect the relative standing of the building for each metric and are visually represented using radar plots, allowing an intuitive comparison across sensors or load conditions.</p>"
    )
  ))


)
