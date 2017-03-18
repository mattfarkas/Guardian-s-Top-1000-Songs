### Import data
top_songs = read.csv("top_songs_more_info.csv", na.strings=c("","NA"))
art.gen = read.csv("artists_and_genres.csv", na.strings=c("","NA"))
total.data = merge(top_songs, art.gen, all.x = T, by.x = "ARTIST", by.y = "artist")

### Define missing genres as 'unidentified'
total.data$master_genre = as.character(total.data$master_genre)
total.data[is.na(total.data$master_genre),'master_genre'] = 'unidentified'
total.data$master_genre = as.factor(total.data$master_genre)
total.data$THEME = NULL
total.data$SPOTIFY_URL = NULL

### Omit na's
complete.data = total.data[complete.cases(total.data),]

### Scale numerical fields
complete.data[,c('danceability', 'valence', 'loudness','liveness', 'tempo', 'instrumentalness',
                 'energy', 'acousticness', 'duration_ms')] = scale(complete.data[,c('danceability', 
                                                'valence', 'tempo', 'loudness', 'liveness', 
                                                'instrumentalness', 'energy', 'acousticness',
                                                'duration_ms')])



### Simple regression on spotify popularity
model.simple.rank = lm(data = complete.data,
                  1/Rank ~ master_genre + YEAR + danceability + valence + tempo + loudness +
                    liveness + converted_keys + instrumentalness + energy + acousticness + 
                    duration_ms)
summary(model.simple.rank)
stepped.rank = step(model.simple.rank, direction = 'both')
summary(stepped.rank)

model.simple.spot.pop = lm(data = complete.data,
                  song_pop ~ master_genre + YEAR + danceability + valence + tempo + loudness +
                    liveness + converted_keys + instrumentalness + energy + acousticness + 
                    duration_ms)
summary(model.simple.spot.pop)

### Stepwise Model
stepped.song.pop = step(model.simple.spot.pop, direction = 'both')
summary(stepped.song.pop)
lm(formula = song_pop ~ master_genre + YEAR + tempo + liveness + 
     instrumentalness + acousticness + duration_ms, data = complete.data)


### Means of popularities per genre
means = aggregate(complete.data[,'song_pop'], list(complete.data$master_genre), mean)

