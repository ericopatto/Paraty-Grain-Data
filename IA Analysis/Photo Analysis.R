library(tidyverse)
library(mmand)
library(loder)

test <- readPng("../Figures/Grain Photos/CSand PNGs/Sediment Photo PY1.png")
testCol <- test[, , 3]
testSmall <- rescale(testCol, 0.5, mnKernel())
# test_thresholded <- threshold(testSmall, 100, method="literal")
# test_thresholded <- threshold(testSmall, method="kmeans")
# 
# display(test_thresholded)
# 
# k <- shapeKernel(c(15,15), type="disc")
# display(testSmall)
# display(test_thresholded)
# display(opening(test_thresholded, k))
# 
# testSmooth <- gaussianSmooth(testSmall, c(3,3))
# display(testSmooth)
# display(threshold(testSmooth, 100, method="literal"))
# display(threshold(testSmooth, method="kmeans"))

background <- gaussianSmooth(testSmall, c(60, 60))
highlights <- gaussianSmooth(testSmall-background, c(30, 30))
#display(background)
#display(highlights)
adjusted <- testSmall - background - highlights  # Adjust the kernel size as needed
#display(testSmall)
#display(adjusted)
adjThr <- threshold(adjusted, -10, method="literal")
#display(adjThr)
adjClose <- closing(adjThr, shapeKernel(c(10,10), type="disc"))
#display(adjClose)
#display(threshold(adjusted, method="kmeans"))

# adjSm <- testSmooth - gaussianSmooth(testSmooth, c(40, 40))  # Adjust the kernel size as needed
# adjSmCl <- threshold(adjSm, .001, method="literal")
# display(adjSmCl)
# display(closing(adjSmCl, shapeKernel(c(13,13), type="disc")))

res <- 2667/0.02

separated <- components(1-adjClose, shapeKernel(c(3,3), type="box"))
sepTib <- separated %>%
  as_tibble() %>%
  mutate(Row = row_number()/res) %>%
  pivot_longer(cols = -(Row), names_to = "Column", values_to = "Index") %>%
  mutate(Column = as.integer(str_sub(Column, 2))/res) %>%
  drop_na()

# sepTib %>%
#   mutate(IndRandom = sample(as.character(1:max(Index)), max(Index))[Index]) %>%
#   ggplot(aes(x = Column, y = Row, fill = IndRandom))+
#   geom_tile()+
#   theme_minimal()+
#   scale_y_reverse()+
#   scale_fill_discrete(guide = "none")+
#   coord_equal()

# display(separated, col=rainbow(max(separated,na.rm=TRUE)))

grainDists <- sepTib %>%
  group_by(Index) %>%
  mutate(B = min(Column) == 1/res | min(Row) == 1/res | max(Column) == ncol(separated)/res | max(Row) == nrow(separated)/res) %>%
  nest() %>%
  rowwise() %>%
  mutate(distMatrices = list(dist(data)),
         nPixels = nrow(data)/res^2,
         maxDist = max(distMatrices),
         minDist = min(distMatrices),
         distRatio = maxDist/minDist,
         Border = any(data["B"])) %>%
  ungroup() %>%
  filter(!Border) %>%
  mutate(Ecc = pi*maxDist^2/nPixels)

grainDists %>%
  unnest(cols = data) %>%
  ggplot(aes(x = Column, y = Row, fill = (Ecc <= 7)))+
  geom_tile()+
  theme_minimal()+
  scale_y_reverse()+
  #scale_fill_viridis_c()+
  coord_equal()

grainDists %>%
  select(Index, nPixels, maxDist, Ecc) #%>%
  #write_csv("../Data/Optical Data/Sediment P11.csv")

# library(EBImages)
# 
# x <- readImage(system.file("images", "shapes.png", package="EBImage"))
# kern <- makeBrush(5, shape='diamond')  
# 
# display(x)
# display(kern, title='Structuring element')
# display(erode(x, kern), title='Erosion of x')
# display(dilate(x, kern), title='Dilatation of x')