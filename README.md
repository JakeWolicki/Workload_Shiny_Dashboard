# NHL Workload R Shiny Dashboard

### Introduction
This dashboard is designed to equip coaches, strength and conditioning (S&C) staff, trainers, and sports scientists with key insights into player readiness and workload. The goal is to foster a deeper understanding of the factors influencing player stress and performance, enabling more effective workload programming to balance on-ice demands. Ultimately, the aim is to reduce injury risk, establish a guided return-to-play (RTP) protocol, and understand the effects of internal and external measures on athlete performance.

### Data Source and Technology
- **Data Source**: Kaggle dataset (link or description of the dataset used)
- **Technology Stack**:  
  - R for analysis  
  - Shiny for interactive dashboard development  
  - Hosted on shinyapps.io with this link [NHL Workload Dashboard]([https://www.shinyapps.io](https://jwolicki.shinyapps.io/NHL_Workload/)) for easy access
 
  - - Hosted on shinyapps.io with this link [NHL Workload Dashboard](https://jwolicki.shinyapps.io/NHL_Workload/) for easy access


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
#### Skater Readiness
- **Tracking Trends**: Monitor player readiness trends to help identify recovery strategies and inform timely adjustments to training programs.
- **Readiness and Strain**: Analyze how readiness correlates with high-strain activities (e.g., high-intensity accelerations and decelerations) and how these translate to on-ice performance.

#### Skater Workload
- **Balancing Workload**: Ensure balanced workload distribution across the team and by position, preventing overtraining or undertraining.
- **Comparative Analysis**: Compare players by position, date, and session type to identify who has the most or least workload.
- **Informed Programming**: Use data to customize training programs, such as reducing eccentric work for players with high deceleration rates to manage stress and prevent injury.

#### Skater Distance Z Scores
- **Analyzing Intensity Levels: This feature breaks down the distance skaters travel across four intensity levels: low, moderate, high, and sprint. To further analyze performance, the skaters' Z-scores are calculated, which show how far each player deviates from the average distance covered by all skaters, for each intensity level.  This is summarized with a table of skaters' Z-scores for each intensity level, followed by four bar charts for easier comparison of Z-scores across low, moderate, high, and sprint intensities.

#### Goalie Readiness
- **Tracking Trends: Similar to the skater readiness section, this feature focuses on goalies. It monitors goalie readiness trends, helping identify recovery strategies and making timely adjustments to their training programs. 

#### Goalie Workload
- **Defensive Zone & Muscle Activation: This section presents a combined bar and line chart. The bar chart shows the time the puck spends in the defensive zone, while the line graphs display the EMG (electromyography) activation for both the right and left quadriceps. 

### Future Vision
- **Injury Correlation**: Pair workload metrics with injury data to identify potential correlations and contributing factors.
- **Injury Prediction**: Work towards identifying methods to mitigate injury risk, even if it results in a marginal reduction.
- **Acute-Chronic Workload Management**: Utilize acute-chronic workload ratios for preseason build-ups, RTP, and ongoing workload management throughout the season.
- **RTP Strategy**: Use workload metrics to guide the RTP process, helping progressively build players back to game-ready status.

