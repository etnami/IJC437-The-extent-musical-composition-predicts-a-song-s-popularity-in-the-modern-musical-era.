# IJC437-Using-Musical-Composition-Features-to-Predict-Song-Popularity-in-the-Modern-Musical-Era: A-Dual-Model-Approach.

### <ins>Overview</ins>
[ introduction and rationale, methodology ]

### <ins>Research Questions</ins>

**RQ1:  Are there any significant relationships between 9 audio features and 1 content feature of a music track and song popularity?**

**RQ2: Do these items interact with each other?**

**RQ3: Is it possible to accurately predict a song's popularity status using music composition variables?**


### <ins>Key Findings</ins>
***RQ1:  Are there any significant relationships between 9 audio features and 1 content feature of a music track and song popularity?***

The Stepwise Regression successfully identified speechiness as a statistically significant negative predictor of popularity. On the other hand, the Random Forest revealed energy and loudness as the most influential features, which were used to reduce Gini impurity and identify popular songs. Explicitness initially showed statistical significance with popularity in a chi-squared test but was ultimately removed by the Stepwise Model, suggesting it is a less significant driver of popularity than acoustic composition. Furthermore, the random forest favoured the acoustics-only model, confirming explicitness does not improve predictive power.

***RQ2: Do these items interact with each other?***

The Pearson Correlation Matrix (Table 8) unveiled high collinearity between loudness and energy; however, the failure of the linear models compared to the relative success of the random forest indicate this relationship is non-linear. The random forest’s feature importance plot (Figure 12) suggests their interaction can reliably be used to predict popularity.

***RQ3: Is it possible to accurately predict a song's popularity status using music composition variables?***

While it is not possible to accurately predict popularity using a standard linear framework, the transition to non-linear modelling demonstrates that a predictive signal does exist within music composition variables.
Logistic and Stepwise Regression models failed to achieve any practicality, returning 0% sensitivity and ultimately acting as majority-class classifiers. However, the Random Forest design successfully bypassed this linear constraint, achieving a 9.03% sensitivity (RF1). These results indicate that song popularity is not determined by individual acoustic traits in isolation, but by complex, non-linear interactions, specifically involving energy and loudness, which allow for a predictive accuracy that exceeds the baseline of random chance, though only marginally.

### <ins>Limitations, Weaknesses, and Assumptions</ins>
<ins>Limitations</ins>

1. There is bias built into the dataset (Silva et al., 2019), as Spotify’s economic and algorithmic weight is heavily skewed towards North Americans because US users lead the engagement metrics (Curry, 2026). Due to amplified engagement from US users, their habits have a louder voice in the algorithm (Prey, 2016; Anderson et al., 2020; Torabi, 2023; Tafradzhiyski, 2025). Consequently, these insights reflect US preferences and listening habits and ignore eastern cultures who use different platforms entirely, like Tencent or JioSaavn (Vidas, McGovern & Nitschinsk, 2021; Duarte, 2025; Research Nester, 2025; Curry, 2026). The same patterns would not be found in a more inclusive dataset, where there is evidence to suggest the acoustic preferences between East and West are different (Tsai 2007; Hakvoort 2015; Liew, Uchida & de Almedia, 2021; Culture and Ideal Affect, 2025). Furthermore, the streaming audience is largely skewed towards Gen Z and Millennial demographics (Fluent, 2017), with 73% of the user base aged 18-34 (Purnell, 2020; Cross River Therapy, 2026). The geographic bias introduces a western-centric generalisation to my findings, especially pertaining to the identified success clusters. This limits the models external validity; when applied to eastern or African markets where there are different acoustic norms and explicitness regulations, they would struggle to perform.

2. The binary is_pop variable used failed to account for the longevity of popularity and whether certain acoustic contributions drive or hinder a song’s duration on the charts.

<ins>Weaknesses</ins>

1. While performing a post-hoc data quality audit and checking for possible duplicates sum(duplicated(dataset_range$song_name)) R identified 414 duplicates Excel had neglected to remove attributed to multi-format releases. This introduced a degree of data redundancy, however, the large sample size in comparison to the affected duplicates meant the models remained robust. Neglecting to check for duplicates in R before beginning analysis was a huge oversight on my part, but retaining the variant forms of songs provided a more authentic representation of the song’s digital footprint because each data entry carried a unique popularity score independent of its duplicate values. Due to these duplicates likely being evenly distributed across popular and non-popular songs, the chi-squared result is unlikely to be hugely shifted. This experience emphasises the importance of removing duplicates in R rather than relying on a manual processing in Excel.

2. The methodological decision to use a dual-model approach placed significant pressure on the word count limit and inflated my methodology and results sections. Consequently, I kept my reporting style concise and only explored the most notable findings. I kept the depth of the project in exchange for narrative detail as this permitted the exploration necessary to make robust conclusions and provide a comprehensive overview of the research.

<ins>Assumptions</ins>

1. Following the lead of existing classification research this project initially assumed linearity in the relationship between acoustic features and popularity. Later findings confirmed lack of linearity hence the switch of classifier to random forest to capture the non-linear interactions, which subsequently improved the predictive performance. The dual-model approach was ultimately decided on because the logistic regression successfully identified a significant predictor and both offered valuable contributions to the research question. The choice to employ both methods allowed for a nuanced and detailed investigation into predictive modelling using musical features.

2. A key assumption of this project was treating popularity as uniform regardless of genre. In reality, however, acoustic profiles differ across genres, with dance pop containing higher danceability than metal (source) and novelty being more rewarded in the rock genre than the pop genre (O’Toole & Horvat, 2023). Moreover, Middlebrook & Sheik (2019) found that the importance of each feature differs depending on genre; reinforcing this, Khan et al., (2022) found the predictive capabilities of speechiness depended on the genre. This means popularity is more reliant on context than treated in this project; a successful song within these categories will have fundamentally different composition. This might explain why the model accuracy scores were so low, because the models were trying to find a one-size-fits-all formula that does not exist. This flawed assumption implies popularity is not a single cluster but a set of sub-clusters specific to each genre. On the other hand, since this project centres mainstream popularity and neglects to delve into niche markets, the findings are still relatively robust, as regardless of genre, these features have been observed obtaining major market success. In this event genre acts as a confounding variable, because one musical feature might be a positive predictor of one genre and a negative predictor for another.

### <ins>Future Work Considerations</ins>
 1.	Addressing artist type, artist fame, and song longevity as predictive variables. My chi-squared for artist type showed significance, however, due to limiting the scope of my project I opted not to include this variable in my final report. Previous research using the same dual-model design identified model improvement when including context and metadata alongside the acoustics (Lee & Lee, 2018; Sharma et al., 2022; Silva et al., 2023; Zhao et al., 2023).Moreover, my model findings emphasised external influence and this project failed to account for the difference between lasting popularity and ‘forgettable’ popularity identified in other work, who found model accuracy increases when this is included (Araujo et al., 2019). This data is critical because trends change rapidly and due to the enhanced saturation of streaming charts, the ability to sustain popularity is likely a better indicator of stronger influence and success, such as The Beatles who maintained their popularity across decades (Mauch et al., 2015; Kaimann, Tanneberg & Cox, 2020). Due to this it can be argued the features identified as being significantly associated with their long term popularity are more valuable metrics, these being higher valence and energy, and low acousticness than most other popular tracks studied in a global dataset (North & Kraus, 2024). Currently, the two most positively corelated with current popularity are loudness and valence, positioning these two features as historical drivers that have lasting impact.
 
  2.	Use of a more inclusive and balanced dataset, expanding to include 2019-2026 listening data, and incorporate diverse metadata from eastern and African streaming platforms to more accurately reflect the differences in listening habits across culture. Sciandra (2024) observed that the predictive power of acousticness and instrumentalness has grown as the internet era matured, emphasising the need to continue research on current-year data.
