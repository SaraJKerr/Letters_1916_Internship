################################################################################
# File-Name: config.R                                                          #
# Date: 7 May 2017                                                             #
# Author: Shane A. McGarry                                                     # 
# ORCID:orcid.org/0000-0002-0312-9163                                          #
# Institution: Maynooth University                                             #
# Project: Letters of 1916 - Prof Susan Schreibman                             #
# Purpose: Strore configuration values needed to run various processes.        #
#          This file can be modified to fit your particular environments needs #                    
# Last Updated:                                                                #
################################################################################

#database configuration variables
config_db_host <- "127.0.0.1"
config_db_port <- 3306
config_db_user <- "letters1916"
config_db_pass <- "l3tTer$1916!"
config_db_dbname <- "letters1916_dse"

#api configuration variables
config_api_baseuri <-"http://127.0.0.1:8081/letters/search"

#text extract & process variables
config_extract_folderpath <- "Text_Files"
config_process_folderpath <- "Processed_Files"
config_treetag_path <- "/users/shanemcgarry/.ref/tree-tagger"
config_results_folderpath <- "Results"