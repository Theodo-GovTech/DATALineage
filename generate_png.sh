#!/bin/bash
mkdir -p procedures/migration-enc-mco
ln -sfn "$(realpath ../sas-lineage-analyzer/tests/e2e/test_data/pgm_sas_2024)" procedures/migration-enc-mco/sas
Rscript bin/trace_lineage.R enc-mco compta compta_exploit2
Rscript bin/generate_operations_graph.R enc-mco procedures/migration-enc-mco/sas/mco.enc.enc.2024.sas compta compta_exploit2 -f dot
dot -Tpng procedures/migration-enc-mco/migration-data/compta/lineage/lineage-graph.dot -o procedures/migration-enc-mco/migration-data/compta/lineage/lineage-graph.png
