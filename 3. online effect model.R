install.packages('RColorBrewer')
library(RColorBrewer)
df <- read.csv('df_small_rate.csv')

str(df)
df <- df[-16,1:8]
cor(df)

names <- colnames(df)

slide_df <- cbind(df[2:15,],df[1:14,])

colnames(slide_df) <- c(names, paste0(names,1))
#slide_df <- slide_df[,c('sales',paste0(names,1))]
round(cor(slide_df),2)
write.csv(slide_df,'slide_df_all.csv',row.names = T)

# 단일 회귀
model_news1 <- lm(sales ~ news_query1, data = slide_df)
summary(model_news1)
predict_news <- predict(model_news1,slide_df)
mean(abs(slide_df$sales - predict_news))

model_news <- lm(sales ~ news_query, data = slide_df)
summary(model_news)
predict_news <- predict(model_news,slide_df)
mean(abs(slide_df$sales - predict_news))

summary(df$sales)


model_video1 <- lm(sales ~ video_count1, data = slide_df)
summary(model_video1)

model_video <- lm(sales ~ video_count, data = slide_df)
summary(model_video)

model_tweets1 <- lm(sales ~ tweets_count1, data = slide_df)
summary(model_tweets1)

model_tweets <- lm(sales ~ tweets_count, data = slide_df)
summary(model_tweets)

model_shop <- lm(sales ~ shop_query, data = slide_df)
summary(model_shop)

# 다변량 2개 선택시
model2_video_tweets <- lm(sales~tweets_count + video_count,data = slide_df)
summary(model2_video_tweets)
### X

model2_video_news1 <- lm(sales~news_query1 + video_count,data = slide_df)
summary(model2_video_news1)

model2_video1_news1 <- lm(sales~news_query1 + video_count1,data = slide_df)
summary(model2_video_news1)
### news only

model2_video1_shop <- lm(sales~video_count1 + shop_query,data = slide_df)
summary(model2_video1_shop)
### video 약간 의미

model3_video1_shop_news1 <- lm(sales~video_count1 + shop_query + news_query1,data = slide_df)
summary(model3_video1_shop_news1)
### news1 만 의미

model3_video1_shop_image <- lm(sales~video_count1 + shop_query + image_query,data = slide_df)
summary(model3_video1_shop_image)
### 세개다 유의미

model3_video1_image_news1 <- lm(sales~video_count1 + image_query + news_query1,data = slide_df)
summary(model3_video1_image_news1)

model3_video1_shop_news1 <- lm(sales~video_count1 + shop_query + news_query1,data = slide_df)
summary(model3_video1_shop_news1)

model2_shop_image <- lm(sales~image_query + shop_query,data = slide_df)
summary(model2_shop_image)

model3_video1_tweets_shop <- lm(sales~video_count1 + tweets_count + shop_query, data = slide_df)
summary(model3_video1_tweets_shop)


round(cor(slide_df),2)
