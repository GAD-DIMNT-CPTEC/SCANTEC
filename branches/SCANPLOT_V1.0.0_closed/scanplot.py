# SCANPLOT - Um sistema de plotagem simples para o SCANTEC
# Copyright (C) 2020 INPE
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""
scanplot
========

    Este módulo contém funções associadas à leitura das informações do namelist do SCANTEC
    e à plotagem das tabelas do SCANTEC (ACOR, RMSE, MEAN e VIES).

Funções
-------
    read_nemalists      : lê os namelists e arquivos de definições do SCANTEC;
    get_dataframe       : transforma as tabelas do SCANTEC em dataframes;
    get_dataset         : transforma os campos com a distribuição espacial das estatísticas do SCANTEC datasets;
    plot_lines          : plota gráficos de linha com os dataframes das tabelas do SCANTEC;
    plot_lines_tStudent : plota gráficos de linha com os dataframes das tabelas do SCANTEC;
    plot_scorecard      : resume as informações dos dataframes com as tabelas do SCANTEC em scorecards;
    plot_dTaylor        : plota diagramas de Taylor a partir de dois experimentos utilizando
                          os dataframes com as tabelas do SCANTEC.
"""

from core_scanplot import read_namelists
from data_structures import get_dataframe, get_dataset
from aux_functions import concat_tables_and_loc, df_fill_nan, calc_tStudent 
from plot_functions import plot_lines, plot_lines_tStudent, plot_scorecard, plot_dTaylor 
from gui_functions import show_buttons
