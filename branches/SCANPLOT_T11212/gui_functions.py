#! /usr/bin/env python3

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

import ipywidgets as widgets
from ipywidgets import interact, GridspecLayout, HBox, VBox, Layout
from IPython.display import display, clear_output, Image

def show_buttons(dvars,dconf):
    
    """
    show_buttons
    ============
    
    Esta função mostra para o usuário uma interface gráfica mínima para a seleção visual 
    das estatísticas, experimentos, variáveis e níveis.
    
    Parâmetros de entrada
    ---------------------
        dvars : objeto dicionário que conterá as variáveis e níveis do SCANTEC de acordo
                com as escolhas do usuário;
        dconf : objeto dicionário que conterá as configurações do SCANTEC de acordo 
                com as escolhas do usuário.
        
    Resultado
    ---------
        Objetos com a interface gráfica mínima e dicionários com as variáveis e configurações
        do SCANTEC.
    
    Uso
    ---
        import scanplot 
        
        data_vars, data_conf = scanplot.read_namelists("~/SCANTEC")
        
        grid, dvars, dconf = scanplot.show_buttons(data_vars, data_conf)
        
    Observações
    -----------
        Experimental, esta função não está finalizada e foi projetada para uso apenas dentro do Jupyter.
    """
    
    data_vars = dvars
    data_conf = dconf
    
    Vars = list(map(dvars.get,[*dvars.keys()]))
    Stats = ["ACOR", "RMSE", "VIES"]
    Exps = list(dconf["Experiments"].keys())
    outDir = dconf["Output directory"]
    
    vexps = list(dconf["Experiments"].keys())
    
    vlist = []
    for i in [*dvars.keys()]:
        vlist.append(dvars[i][0])
    
    style = {'description_width': 'initial'}
    
    SMVars = widgets.SelectMultiple(
        options=vlist,
        value=[vlist[0]],
        layout={'width': '30%'},    
        description='Variável(is) e Nível(is):',
        disabled=False,
        style = style
    )

    SMStats = widgets.SelectMultiple(
        options=['ACOR', 'RMSE', 'MAE', 'VIES'],
        value=['ACOR'],
        layout={'width': '30%'}, 
        description='Estatística(s):',
        disabled=False,
        style = style
    )
    
    SMExps = widgets.SelectMultiple(
        options=vexps,
        value=[vexps[0]],
        layout={'width': '30%'}, 
        description='Experimento(s):',
        disabled=False,
        style = style
    )
    
    CaixaTexto = widgets.HTML(
        value="Selecione as opções a seguir e clique no botar Salvar para guardar a seleção ou no botão Limpar para limpar a seleção.",
        placeholder='Some HTML',
        description='',
    )
    
    Salvar = widgets.Button(
        description='Salvar',
        disabled=False,
        button_style='success', 
        tooltip='Clique para salvar a seleção',
        icon='' 
    )

    Resetar = widgets.Button(
        description='Resetar',
        disabled=False,
        button_style='warning', 
        tooltip='Clique para resetar a seleção',
        icon='' 
    )
    
    out = widgets.Output()

    box_layout = Layout(display='flex',
                    flex_flow='row',
                    align_items='stretch',
                    justify_content='center',
                    border='1px dotted #000000',
                    padding='10px',
                    margin='5px', 
                    width='99%')
    
    def on_reset_button_clicked(b):
        Resetar.description = 'Limpar'
        Resetar.button_style = 'warning'
        with out:
            clear_output()
    
    def on_save_button_clicked(change):
        Salvar.description = 'Salvar'
        Salvar.button_style = 'success'
        with out:
            clear_output()
            expsDict = {}
            print(list(SMExps.value))
            print(dconf)
            for exp in list(SMExps.value):
                expsDict[exp] = list(dconf["Experiments"][exp])
                print(exp,expsDict)
            data_conf["Experiments"] = expsDict
        
    Salvar.on_click(on_save_button_clicked)
    Resetar.on_click(on_reset_button_clicked)
    
    top_box = VBox(children=[CaixaTexto], layout=box_layout)
    middle_box = HBox(children=[SMStats, SMExps, SMVars], layout=box_layout)
    bottom_box = HBox(children=[Salvar, Resetar], layout=box_layout)
    tab_opts = VBox(children=[top_box, middle_box, bottom_box, widgets.HBox([out])])

    tab = widgets.Tab(children=[tab_opts])
    tab.set_title(0, 'Opções')

    return VBox(children=[tab]), data_vars, data_conf
