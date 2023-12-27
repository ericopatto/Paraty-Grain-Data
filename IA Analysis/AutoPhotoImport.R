library(tidyverse)
library(proxy)
library(loder)
library(mmand)
library(fs)

folder_path <- "../Figures/Grain Photos/CSand PNGs"

filenames <- map_chr(dir(folder_path, pattern = "*.png"), ~paste0(folder_path, "/", .))

images <- map(filenames, ~rescale(readPng(.)[, , 3], 0.5, mnKernel()))

names(images) <- str_sub(basename(filenames), -7, -5)

imgBacks <- map(images, ~ {
  .[. < 100] <- 0
  .
})

imgBackBool <- map(images, ~ {
  .[. < 100] <- 0
  .[. >= 100] <- 1
  .
})

mean <- reduce(imgBacks, `+`) / reduce(imgBackBool, `+`)

imgTreat <- map(images, ~(.-mean))

imgClose <- map(imgTreat, ~closing(threshold(., -10, method="literal"), shapeKernel(c(10,10), type="disc")))

imgEdge <- map(imgClose, ~{dilate(.,shapeKernel(c(3,3), type="disc")) - erode(.,shapeKernel(c(3,3), type="disc"))})

# map(imgClose, display)

res <- 2667/0.02 * 0.5

nCol <- ncol(images[[1]])
nRow <- nrow(images[[1]])

grainFill <- map(imgClose, ~{
  components(1-., shapeKernel(c(3,3), type="box")) %>%
    as_tibble() %>%
    mutate(Row = row_number()/res) %>%
    pivot_longer(cols = -(Row), names_to = "Column", values_to = "Index") %>%
    mutate(Column = as.integer(str_sub(Column, 2))/res) %>%
    drop_na() %>%
    group_by(Index) %>%
    mutate(B = min(Column) == 1/res | min(Row) == 1/res | max(Column) == nCol/res | max(Row) == nRow/res)
})

grainEdges <- map(imgEdge, ~{
  as_tibble(.) %>%
    mutate(Row = row_number()/res) %>%
    pivot_longer(cols = -(Row), names_to = "Column", values_to = "Edge") %>%
    mutate(Column = as.integer(str_sub(Column, 2))/res) %>%
    filter(Edge == 1)
})

grainMaxDist <- map2(.x = grainFill, .y = grainEdges, .f = ~{
  edges <- inner_join(.x, .y) %>% # group_by(Index) %>%
    select(Row, Column, Index)
  pos2 <- edges %>%
    summarise(RowRef = mean(.x$Row), ColumnRef = mean(.x$Column), Index = first(Index), Distance = 1)
  
  # prevRow2 <- mean(.x$Row)
  # prevCol2 <- mean(.x$Column)
  
  for (i in 1:1e1) {
    pos1 <- edges %>%
      left_join(pos2) %>%
      mutate(Distance = (Row - RowRef)^2 + (Column - ColumnRef)^2) %>%
      filter(Distance == max(Distance)) %>%
      summarise(Index = first(Index), RowRef = first(Row), ColumnRef = first(Column), Distance = first(Distance))
    pos2 <- edges %>%
      left_join(pos1) %>%
      mutate(Distance = (Row - RowRef)^2 + (Column - ColumnRef)^2) %>%
      filter(Distance == max(Distance)) %>%
      summarise(Index = first(Index), RowRef = first(Row), ColumnRef = first(Column), Distance = first(Distance))
    dist <- pos1 %>%
      inner_join(pos2, by = c("Index")) 
    if(all(dist$Distance.x == dist$Distance.y)) break()
  }
  dist %>%
    transmute(Index, Distance = sqrt(Distance.x*(Distance.x>=Distance.y) + Distance.y*(Distance.x<Distance.y)))
})

grainData <- map2(.x = grainMaxDist, .y = grainFill, .f =
                    ~{inner_join(.x, .y %>%
                                   group_by(Index) %>%
                                   summarise(nPixels = n()/res^2,
                                             Border = any(B)) %>%
                                   filter(!Border) %>%
                                   ungroup() ) %>%
                        
                        mutate(Ecc = pi*Distance^2/(4*nPixels))
                    }
)

grainFullData <- imap(.x = grainData, .f = 
      \(x, idx){
        inner_join(x, grainFill[[idx]]) %>%
          select(-B, -Border)
      })

# map(names(grainFullData), ~write_csv(x = grainFullData[[.]], file = paste0("../Figures/Grain Photos/Processed Data/Full Data/", ., ".csv")))

maps <- imap(.x = grainFullData, .f =
              \(x, idx){x %>%
                  ggplot(aes(x = Column, y = Row, fill = Ecc<2.15))+
                  geom_tile()+
                  theme_minimal()+
                  scale_fill_discrete(breaks = c(TRUE, FALSE), labels = c("Low", "High"), name = "Eccentricity")+
                  coord_equal()+
                  scale_y_reverse(labels = function(x){paste0(x, " m")})+
                  scale_x_continuous(labels = function(x){paste0(x, " m")})+
                  labs(title = paste0("Grain selection of Sample ", idx), x = "", y = "")
              })

# map(.x = names(maps), .f = ~ggsave(maps[[.x]], file = paste0("../Figures/Plots/Grain Map/", .x, ".png"), width = 2848/2, height = 4288/2-500, units = "px"))
