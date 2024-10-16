# NHL Workload R Shiny Dashboard

### Introduction
This dashboard is designed to equip coaches, strength and conditioning (S&C) staff, trainers, and sports scientists with key insights into player readiness and workload. The goal is to foster a deeper understanding of the factors influencing player stress and performance, enabling more effective workload programming to balance on-ice demands. Ultimately, the aim is to reduce injury risk, establish a guided return-to-play (RTP) protocol, and understand the effects of internal and external measures on athlete performance.

### Data Source and Technology
- **Data Source**: Kaggle dataset (link or description of the dataset used)
- **Technology Stack**:  
  - R for analysis  
  - Shiny for interactive dashboard development  
  - Hosted on [shinyapps.io](https://www.shinyapps.io) for easy access

### Goals
- **Understanding Player Stressors**: Gain insights into the stressors affecting players and their contributing factors.
- **Optimizing Workload**: Fine-tune programming by identifying players who may need increased or reduced workloads based on on-ice demands.
- **Reducing Injury Risk**: Leverage workload data to make decisions that lower injury risk.
- **Creating a Return-to-Play (RTP) Protocol)**: Develop data-driven protocols to ensure a safe and efficient RTP process.

### Target Audience
- Coaches
- Strength & Conditioning (S&C) Staff
- Trainers
- Sports Science Teams

### Features
#### Player Workload
- **Tracking Trends**: Monitor player readiness trends to help identify recovery strategies and inform timely adjustments to training programs.
- **Readiness and Strain**: Analyze how readiness correlates with high-strain activities (e.g., high-intensity accelerations and decelerations) and how these translate to on-ice performance.

#### Team Workload
- **Balancing Workload**: Ensure balanced workload distribution across the team and by position, preventing overtraining or undertraining.
- **Comparative Analysis**: Compare players by position, date, and session type to identify who has the most or least workload.
- **Informed Programming**: Use data to customize training programs, such as reducing eccentric work for players with high deceleration rates to manage stress and prevent injury.

### Future Vision
- **Injury Correlation**: Pair workload metrics with injury data to identify potential correlations and contributing factors.
- **Injury Prediction**: Work towards identifying methods to mitigate injury risk, even if it results in a marginal reduction.
- **Acute-Chronic Workload Management**: Utilize acute-chronic workload ratios for preseason build-ups, RTP, and ongoing workload management throughout the season.
- **RTP Strategy**: Use workload metrics to guide the RTP process, helping progressively build players back to game-ready status.

### Installation Instructions
1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/NHL-Workload-Shiny-Dashboard.git
   ```
2. Install the required R packages:
   ```r
   install.packages(c("shiny", "ggplot2", "dplyr", "plotly"))  # Include all necessary libraries
   ```
3. Run the Shiny app locally:
   ```r
   shiny::runApp('path/to/your/app')
   ```

### Usage
Once installed, the dashboard can be accessed locally or hosted online. Simply run the app and interact with various filters to view player-specific workload, readiness scores, and team-wide insights.

### Contribution Guidelines
If you would like to contribute to this project:
1. Fork the repository.
2. Create a new branch (`git checkout -b feature-branch-name`).
3. Make your changes and commit them (`git commit -m 'Add new feature'`).
4. Push to the branch (`git push origin feature-branch-name`).
5. Submit a pull request.

### License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
