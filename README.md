# treeRchitecture
This repo calculates tree architecture metrics from cylinder models (QSMs) in R. 

treeRchitecture_example.R provides and example workflow.
treeRchitecture_functions.R provides the necessary functions.
example_data provides some QSMs to work with initially (currently in .mat format).

The QSMs must be built using Pasi Raumonen's TreeQSM https://github.com/InverseTampere/TreeQSM and the functions here are built from (and may be merged with) Allie Shenkins treestruct https://github.com/ashenkin/treestruct.

These functions can be used to explore architectural differences between trees, as in Verbeeck, Hans, et al. "Time for a plant structural economics spectrum." Frontiers in Forests and Global Change 2 (2019): 43.

The mechanical stability functions are similar to those used in Jackson, Tobias D., et al. "The mechanical stability of the world’s tallest broadleaf trees." Biotropica 53.1 (2021): 110-120.

Note that these functions only work if you have cylinder models (QSM) trees. If you have plot level point cloud data you need to extract individual trees (manually or with https://github.com/apburt/treeseg), remove leaves https://pypi.org/project/tlseparation/, and fit a QSM (https://github.com/InverseTampere/TreeQSM). Other packages work with point cloud data and I would recommend https://github.com/TobyDJackson/treeRchitecture/
