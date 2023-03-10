library(tidyverse)
library(spotifyr)
library(plotly)
library(compmus)

df <- get_playlist_audio_features("", "7KtmJXjsDmw8WEdtyL28L3?si=bc8137fcca564e11")

conditions <- c("QUARTIERS NORD", "Les dernières volontés de Mozart (Symphony)", "Cœur blanc", "Deux frères", 
                "L'EMPIRE DE MÉROÉ", "Dans la légende", "Capo Dei Capi Vol. II & III")
df$language <- ifelse(df$track.album.name %in% conditions, "French", "Dutch")

df_dutch <- df[df$language == "Dutch", ]
df_french <- df[df$language == "French", ]
 
cm23 <-
  bind_rows( 
    df_french |> mutate(category = "French"),
    df_dutch |> mutate(category = "Dutch")
  )

###############################PLOT_1###########################################
p1 <- cm23 %>%
  mutate(
    mode = ifelse(mode == 0, "Minor", "Major")
  ) %>%
  ggplot(
    aes(
      x = tempo, 
      y = danceability,
      size = energy,
      colour = mode
    )
  ) +
  geom_point() +
  geom_rug(linewidth = 0.2) +
  facet_wrap(~ category) +
  scale_x_continuous(
    limits = c(50, 200),
    breaks = c(50, 125, 200),
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_brewer(
    type = "qual",
    palette = "Dark2"
  ) +
  scale_size_continuous(
    trans = "exp",
    #guide = "none",
    range = c(0, 2)
  ) + 
  theme_bw() +
  labs(
    x = "Tempo",
    y = "Danceability",
    colour = "Mode"
  ) + ggtitle("Relationship between tempo, danceability, energy in French & Dutch Hip hop")


###############################PLOT_2###########################################
p2 <- ggplot(cm23, mapping = aes(x = valence, y = energy, size=loudness)) +
  geom_boxplot(alpha = 0.5) + 
  geom_jitter(alpha = 0.5, color = "black") + 
  facet_wrap(~ category) +
  scale_size_continuous(
    trans = "exp",
    guide = "none",
    range = c(1, 5)) +
  ggtitle("Relationship between valence, energy and loudness in French & Dutch Hip hop") +
  theme_bw()

 ggplotly(p1)

################################################################################
 wood <-
   get_tidy_audio_analysis("6H8njIysTyNsjD5CyPtqwN") |>
   select(segments) |>
   unnest(segments) |>
   select(start, duration, pitches)
 
 wood |>
   mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
   compmus_gather_chroma() |> 
   ggplot(
     aes(
       x = start + duration / 2,
       width = duration,
       y = pitch_class,
       fill = value
     )
   ) +
   geom_tile() +
   labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
   theme_minimal() +
   scale_fill_viridis_c()
 
 ################################################################################
 wood <-
   get_tidy_audio_analysis("5y2WMZ9UAYsA7qxRENID6l") |>
   select(segments) |>
   unnest(segments) |>
   select(start, duration, pitches)

 wood |>
   mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
   compmus_gather_chroma() |> 
   ggplot(
     aes(
       x = start + duration / 2,
       width = duration,
       y = pitch_class,
       fill = value
     )
   ) +
   geom_tile() +
   labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
   theme_minimal() +
   scale_fill_viridis_c()
 
 ################################################################################
 





