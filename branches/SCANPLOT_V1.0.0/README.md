# SCANPLOT - Um sistema de plotagem simples para o SCANTEC

O SCANPLOT é um módulo escrito em linguagem Python preparado para ler e plotar as tabelas com as estatísticas do Sistema Comunitário de Avaliação de modelos Numéricos de Tempo e Clima (SCANTEC). O seu uso pode ser feito por meio da linha de comando ou através do Jupyter Notebook. O SCANPLOT transforma as tabelas do SCANTEC em dataframes do Pandas e pode ser facilmente extendido a partir da introdução de funções para a plotagem destes dataframes na forma como o usuário precisar.

Esta versão do `scanplot` está organizada da seguinte forma:

1. `core_scanplot`: contém funções relacionadas com a leitura dos arquivos de configuração do SCANTEC;
2. `data_structures`: contém funções relacionadas com as estruturas de dados utilizadas pelo SCANPLOT;
3. `aux_functions`: contém funções auxiliadas utilizadas por outras partes do módulo;
4. `plot_functions`: contém funções relacionadas com a plotagem as estruturas de dados do SCANPLOT;
5. `gui_functions`: contém funções relacionadas com as widgets do Jupyter Notebook.

As principais funções do módulo são as seguintes:

1. `read_namelists`: esta função lê os arquivos de namelist e definições dos modelos do SCANTEC;
2. `get_dataframe`: esta função transforma uma ou mais tabelas em dataframes do Pandas, acessíveis por meio de um dicionário;
3. `plot_lines`: esta função plota gráficos de linhas a partir dos dataframes;
3. `plot_lines_tStudent`: esta função plota gráficos de linhas a partir dos dataframes, acompanhadas com o teste de significância t de Student;
4. `plot_scorecard`: esta função plota um scorecard a partir dos dataframes;
5. `plot_dTaylor`: esta função plota um diagrama de Taylor a partir dos dataframes.

A documentação do SCANPLOT pode ser encontrada em https://cfbastarz.github.io/SCANPLOT/. Um notebook do Jupyter com a aplicação das funções do SCANPLOT, está disponível em https://github.com/cfbastarz/SCANPLOT/blob/master/SCANPLOT.ipynb
