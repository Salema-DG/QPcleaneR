
#' @title Sector Crosswalk for QP
#'
#' @description
#' A sector crosswalk of sector at the 2 digit level. Creates the column "sector".
#'
#' Crosswalk based on Pedro Raposo's files.
#'
#' The classification is the following:
#'
#' |Code | Industry
#' |--|--|
#' |1 | Agriculture, Animal Production, Hunt e Silvicultura|
#' |2 | Pesca|
#' |3 | Extracção de Produtos Energéticos|
#' |4 | "Indústrias Extractivas com Excepção da Extracção de Produtos Energéticos" |
#' |5 | "Indústrias Alimentares, das Bebidas e do Tabaco"|
#' |6 | "Indústria Têxtil"|
#' |7 | "Indústria do Couro e dos Produtos de Couro"|
#' |8 | "Indústrias da Madeira e da Cortiça e suas Obras"|
#' |9 | "Indústrias de Pasta de Papel e Cartão e seus Artigos, Edição e Impressão"|
#' |10 | "Fabricação de Coque, Produtos Petrolíferos Refinados e Combustível Nuclear"|
#' |11 | "Fabricação de Produtos Químicos e de Fibras Sintéticas ou Artificiais"|
#' |12 | "Fabricação de Artigos de Borracha e de Matérias Plásticas"|
#' |13 | "Fabricação de Outros Produtos Minerais Não Metálicos"|
#' |14 | "Indústrias Metalúrgicas de Base e de Produtos Metálicos"|
#' |15 | "Fabricação de Máquinas e de Equipamentos N.E."|
#' |16 | "Fabricação de Equipamento Eléctrico e de Óptica"|
#' |17 | "Fabricação de Material de Transporte"|
#' |18 | "Indústrias Transformadoras N.E."|
#' |19 | "Produção e Distribuição de Electricidade, de Gás e de Água"|
#' |20 | "Construção"|
#' |21 | "Comércio por Grosso e a Retalho; Reparação de Veículos e de Bens de Uso Pessoal"|
#' |22 | "Alojamento e Restauração (Restaurantes e Similares)"|
#' |23 | "Transportes, Armazenagem e Comunicações"|
#' |24 | "Actividades Financeiras"|
#' |25 | "Actividades Imobiliárias, Alugueres e Serviços Prestados às Empresas"|
#' |26 | "Administração Publica, Defesa e Segurança Social Obrigatória"|
#' |27 | "Educação" |
#' |28 | "Saúde e Acção Social"|
#' |29 | "Outras Actividades de Serviços Colectivos, Sociais e Pessoais"|
#' |30 | "Famílias com Empregados Domésticos"|
#' |31 | "Organismos Internacionais e Outras Instituições Extra-territoriais"|
#'
#' Sector/Industry CAE Revisions:
#'
#' | Name | Years |
#' |--|--|
#' | Rev 1 | (1982-1994) |
#' | Rev 2 | (1995-2002) |
#' | Rev 2.1 | (2003-2006) |
#' | Rev 3 | 2007-today |
#'
#' To the 2 digits agregation level, there is no difference between Rev 2 and Rev 2.1.
#'
#'
#' @param data A tibble.
#' @param year Not a string. Name of the year column in data.
#' @param original_3d_cae Not a string. Name of the column with the original 3 digit CAE.
#' @param new_variable Name of the new variable. Default is "sector".
#'
#' @return A tibble with the column "sector", consistent throughout the years.
#'
#'

crosswalk_sector <- function(data,
                             year,
                             original_3d_cae,
                             new_variable = sector) {

  # DEFUSE (return without evaluate) AND INJECT
  data %<>% dplyr::mutate(ano = {{ year }},
                   sector_3d = {{ original_3d_cae }} )

  # create 1d and 2d cae variables
  data %<>% dplyr::mutate(sector_1d = {{ original_3d_cae }} %>% stringr::str_sub(1, 1),
                   sector_2d = {{ original_3d_cae }} %>% stringr::str_sub(1, 2))

  data %<>%
    dplyr::mutate( {{ new_variable }} := dplyr::case_when( # if I do = , and not :=, I cannot set the name dynamically

      # 1st case, for CAE Rev 3, from 2004 onwards:
      ano>=2007 & (sector_2d == "01" | sector_2d == "02") ~ 1,
      ano>=2007 & sector_2d== "03" ~ 2,
      ano>=2007 & (sector_2d=="05" | sector_2d=="06") ~ 3,
      ano>=2007 & ((sector_2d=="07") | sector_2d=="08" | sector_2d=="09") ~ 4,
      ano>=2007 & (sector_2d=="10" | sector_2d=="11" | sector_2d=="12") ~ 5,
      ano>=2007 & (sector_2d=="13" | sector_2d=="14") ~ 6,
      ano>=2007 & (sector_2d=="15") ~ 7,
      ano>=2007 & (sector_2d=="16") ~ 8,
      ano>=2007 & (sector_2d=="17" | sector_2d=="18" | sector_2d=="58") ~ 9,
      ano>=2007 & (sector_2d=="19") ~ 10,
      ano>=2007 & ((sector_2d=="20") | sector_2d=="21") ~ 11,
      ano>=2007 & (sector_2d=="22") ~ 12,
      ano>=2007 & sector_2d=="23"  ~ 13,
      ano>=2007 & (sector_2d=="24" | sector_2d=="25") ~ 14,
      ano>=2007 & (sector_2d=="28" | sector_2d=="33") ~ 15,
      ano>=2007 & (sector_2d=="26" | sector_2d=="27" | sector_3d=="325" ) ~ 16,
      ano>=2007 & (sector_2d=="29" | sector_2d=="30") ~ 17,
      ano>=2007 & (sector_2d=="31" | (sector_2d=="32" & sector_3d!="325") | sector_3d=="383") ~ 18,
      ano>=2007 & (sector_2d=="35" | sector_2d=="36") ~ 19,
      ano>=2007 & (sector_2d=="41" | sector_2d=="42" | sector_2d=="43") ~ 20,
      ano>=2007 & (sector_2d=="45" | sector_2d=="46" | sector_2d=="47" | sector_2d=="95") ~ 21,
      ano>=2007 & (sector_2d=="55" | sector_2d=="56") ~ 22,
      ano>=2007 & (sector_2d=="49" | sector_2d=="50" | sector_2d=="51" | sector_2d=="52" | sector_2d=="53" | sector_2d=="61") ~ 23,
      ano>=2007 & (sector_2d=="64" | sector_2d=="65" | sector_2d=="66")  ~ 24,
      ano>=2007 & (sector_2d=="60" | sector_2d=="62" | sector_2d=="63" | sector_2d=="68" | sector_2d=="69" | sector_2d=="70" | sector_2d=="71" | sector_2d=="72" | sector_2d=="73" | sector_2d=="74" | sector_2d=="77" | sector_2d=="78" | sector_2d=="79" | sector_2d=="80" | sector_2d=="81" | sector_2d=="82") ~ 25,
      ano>=2007 & sector_2d=="84" ~ 26,
      ano>=2007 & sector_2d=="85" ~ 27,
      ano>=2007 & (sector_2d=="75" | sector_2d=="86" | sector_2d=="87" | sector_2d=="88") ~ 28,
      ano>=2007 & (sector_2d=="37" | (sector_2d=="38" & sector_3d!="383") | sector_2d=="39" | sector_2d=="59" | sector_2d=="90" | sector_2d=="91" | sector_2d=="92" | sector_2d=="93" | sector_2d=="94" | sector_2d=="96") ~ 29,
      ano>=2007 & sector_2d=="97" ~ 30,
      ano>=2007 & sector_2d=="99" ~ 31,

      # 2nd case, for CAE Rev 2 and 2.1, from 1995 to 2006
      (ano>=1995  & ano<=2006) & (sector_2d=="01" | sector_2d=="02") ~ 1,
      (ano>=1995  & ano<=2006) & sector_2d=="05" ~ 2,
      (ano>=1995  & ano<=2006) & (sector_2d=="10" | sector_2d=="11" | sector_2d=="12") ~ 3,
      (ano>=1995  & ano<=2006) & (sector_2d=="13" | sector_2d=="14") ~ 4,
      (ano>=1995  & ano<=2006) & (sector_2d=="15" | sector_2d=="16") ~ 5,
      (ano>=1995  & ano<=2006) & (sector_2d=="17" | sector_2d=="18") ~ 6,
      (ano>=1995  & ano<=2006) & sector_2d=="19" ~ 7,
      (ano>=1995  & ano<=2006) & sector_2d=="20" ~ 8,
      (ano>=1995  & ano<=2006) & (sector_2d=="21" | sector_2d=="22") ~ 9,
      (ano>=1995  & ano<=2006) & sector_2d=="23" ~ 10,
      (ano>=1995  & ano<=2006) & sector_2d=="24" ~ 11,
      (ano>=1995  & ano<=2006) & sector_2d=="25" ~ 12,
      (ano>=1995  & ano<=2006) & sector_2d=="26" ~ 13,
      (ano>=1995  & ano<=2006) & (sector_2d=="27" | sector_2d=="28") ~ 14,
      (ano>=1995  & ano<=2006) & sector_2d=="29" ~ 15,
      (ano>=1995  & ano<=2006) & (sector_2d=="30" | sector_2d=="31" | sector_2d=="32" | sector_2d=="33") ~ 16,
      (ano>=1995  & ano<=2006) & (sector_2d=="34" | sector_2d=="35") ~ 17,
      (ano>=1995  & ano<=2006) & (sector_2d=="36" | sector_2d=="37") ~ 18,
      (ano>=1995  & ano<=2006) & (sector_2d=="40" | sector_2d=="41") ~ 19,
      (ano>=1995  & ano<=2006) & sector_2d=="45" ~ 20,
      (ano>=1995  & ano<=2006) & (sector_2d=="50" | sector_2d=="51" | sector_2d=="52") ~ 21,
      (ano>=1995  & ano<=2006) & sector_2d=="55" ~ 22,
      (ano>=1995  & ano<=2006) & (sector_2d=="60" | sector_2d=="61" | sector_2d=="62" | sector_2d=="63" | sector_2d=="64") ~ 23,
      (ano>=1995  & ano<=2006) & (sector_2d=="65" | sector_2d=="66" | sector_2d=="67") ~ 24,
      (ano>=1995  & ano<=2006) & (sector_2d=="70" | sector_2d=="71" | sector_2d=="72" | sector_2d=="73" | sector_2d=="74") ~ 25,
      (ano>=1995  & ano<=2006) & sector_2d=="75" ~ 26,
      (ano>=1995  & ano<=2006) & sector_2d=="80" ~ 27,
      (ano>=1995  & ano<=2006) & sector_2d=="85" ~ 28,
      (ano>=1995  & ano<=2006) & (sector_2d=="90" | sector_2d=="91" | sector_2d=="92" | sector_2d=="93") ~ 29,
      (ano>=1995  & ano<=2006) & sector_2d=="95" ~ 30,
      (ano>=1995  & ano<=2006) & sector_2d=="99" ~ 31,

      # 3rd case, for CAE 73 (rev 1), from 1985 to 1994
      ano<=1994 & (sector_2d=="11" | sector_2d=="12") ~ 1,
      ano<=1994 & sector_2d=="13" ~ 2,
      ano<=1994 & (sector_2d=="21" | sector_2d=="22") ~ 3,
      ano<=1994 & (sector_2d=="23" | sector_2d=="29") ~ 4,
      ano<=1994 & sector_2d=="31" ~ 5,
      ano<=1994 & (sector_3d=="321" | sector_3d=="322") ~ 6,
      ano<=1994 & (sector_3d=="323" | sector_3d=="324") ~ 7,
      ano<=1994 & sector_3d=="331" ~ 8,
      ano<=1994 & sector_2d=="34" ~ 9,
      ano<=1994 & sector_3d=="353" ~ 10,
      ano<=1994 & (sector_3d=="351" | sector_3d=="352" | sector_3d=="354") ~ 11,
      ano<=1994 & (sector_3d=="355" | sector_3d=="356") ~ 12,
      ano<=1994 & (sector_2d=="36") ~ 13,
      ano<=1994 & (sector_2d=="37" | sector_3d=="381") ~ 14,
      ano<=1994 & sector_3d=="382" ~ 15,
      ano<=1994 & (sector_3d=="383" | sector_3d=="385") ~ 16,
      ano<=1994 & sector_3d=="384" ~ 17,
      ano<=1994 & (sector_2d=="39" | sector_3d=="332") ~ 18,
      ano<=1994 & sector_1d=="4" ~ 19,
      ano<=1994 & sector_2d=="50" ~ 20,
      ano<=1994 & (sector_2d=="61" | sector_2d=="62" | sector_3d=="951") ~ 21,
      ano<=1994 & sector_2d=="63" ~ 22,
      ano<=1994 & (sector_2d=="71" | sector_2d=="72") ~ 23,
      ano<=1994 & (sector_2d=="81" | sector_2d=="82") ~ 24,
      ano<=1994 & (sector_2d=="83" | sector_3d=="932" | sector_2d=="92") ~ 25,
      ano<=1994 & sector_2d=="91" ~ 26,
      ano<=1994 & sector_3d=="931" ~ 27,
      ano<=1994 & (sector_3d=="933" | sector_3d=="934") ~ 28,
      ano<=1994 & (sector_3d=="935" | sector_3d=="939" | sector_2d=="94" | sector_3d=="952" | sector_3d=="953" | sector_3d=="959") ~ 29,
      ano<=1994 & sector_2d=="96" ~ 30,
      # this last category is misc. Some sectors in this crosswalks don't well fir anywhere.
      TRUE ~ 0
    ))

  # eliminate useless columns
  data %<>% dplyr::select(!c(ano,
                      sector_3d,
                      sector_2d,
                      sector_1d))

  return(data)

}

