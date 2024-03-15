## Extinction drives recent thermophilization but does not trigger homogenization in forest understorey

### Abstract:

The ongoing climate change is triggering plant community thermophilization. This selection process ought to shift community composition towards species adapted to warmer climates but may also lead to biotic homogenization. The link between thermophilization and homogenization and the community dynamics that drive them (colonization and extinction) remain unknown but is critical for understanding community responses under rapid environmental change. 
We used 14,167 pairs of plots to study shifts in plant community during 10 years of rising temperature in 80 forest ecoregions of France. We computed community mean thermal optimum (thermophilization) and Δβ-diversity (homogenization) for each ecoregion and partitioned these changes into extinction and colonization dynamics of cold- and warm-adapted species. 
Forest understorey communities thermophilized on average by 0.12 °C per decade and up to 0.20 °C per decade in warm ecoregions. This rate was entirely driven by extinction dynamics. Extinction of cold-adapted species was a driver of homogenization but it was compensated for by the colonization of rare species and the extinction of common species, resulting in the absence of an apparent homogenization trend. 
Here we show a dieback of present cold-adapted species rather than an adaptation of communities via the arrival of warm-adapted species, with a mutually cancelling effect on β-diversity. These results suggest that a future loss of biodiversity and delayed biotic homogenization should be considered.

**DOI:** https://doi.org/10.1038/s41559-024-02362-3


This repo contains the raw dataset and the code used in the study "Extinction drives recent thermophilization but does not trigger homogenization in forest understory " published in *Nature Ecology & Evolution*

You can clone this R project to reproduce the Analysis, the results and the figures, we advise you to use R studio and its Git connection.

tutorial to getting stated with R studio and git :https://jennybc.github.io/2014-05-12-ubc/ubc-r/session03_git.html

The dataset contains all of the French Forest Inventory (new version: 2005-2021), which is freely distributed  by the institute for geographic and forest information (IGN) at https://inventaire-forestier.ign.fr/

We included subsets of the rasters in order to reproduce the figures
The analysis was run using R `4.1.1`, and we checked for reproducibility with R `3.6.2`
