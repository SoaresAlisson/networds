library(sto)
# text copied after build
build <- "collapse_adp connectors cooccur count_graphs count_vec entity_list_2_graph entity_list_2_graph2 extract_entities extract_entities2 extract_entities_l extract_entities_v extract_entity extract_graph extract_graph_df extract_graph_pos extract_graph_rgx extract_nodes extract_relation extract_triplets filter_by_query filter_ego filter_ppn gget_cooc get_cooc_entities get_entities get_graph_from_txt get_neighbors get_neighbors_df get_node_id graph_from_cooccurrence graph_subs group_entities group_ppn group_seq_pos net_wordcloud parsePOS plot_graph plot_graph2 plot_graph_i plot_pos_graph q_plot rename_cols split_graph subs_ppn text_sample tokenize_words txt_wiki viz_triplets_i"  |> strsplit(" ")  |>  unlist()

inYml <- readLines("_pkgdown.yml") |> grep2("\\:", invert = T)  |>  grep2("#", invert = T) |> strsplit(" ") |> unlist()  |> unique()  |>  grep2("^-$", invert = T) |> stringi::stri_remove_empty()

build[!build %in% inYml]
