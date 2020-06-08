#! /usr/bin/env pythonw
#-*- encoding: utf-8 -*-

"""
Uso: ./interface.py
"""

import wx
import os
from wx.lib.mixins import rubberband

#provider = wx.SimpleHelpProvider()
#wx.HelpProvider_Set(provider)

class Editor(wx.Frame):
    def __init__(self,parent, id, title):
        wx.Frame.__init__(self, parent, id, title, size=(500,400))

       	self.modify = False
       	self.last_name_saved = ''
        self.replace = False

        self.control = wx.TextCtrl(self, 1, style=wx.TE_MULTILINE)
        self.CreateStatusBar()

        menubaredit = wx.MenuBar()

       	arquivoedit = wx.Menu()
        salvaredit = wx.MenuItem(arquivoedit, 103, '&Salvar\tCtrl+S', 'Salvar arquivo')
        arquivoedit.Append(salvaredit)

        sairedit = wx.MenuItem(arquivoedit, 105, 'Sa&ir\tCtrl+Q', 'Fechar o Avaliador')
        arquivoedit.Append(sairedit)

        menubaredit.Append(arquivoedit, '&Arquivo')

        self.Bind(wx.EVT_MENU, self.OnSaveFile, id=103)
        self.Bind(wx.EVT_MENU, self.QuitApplication, id=105)

        self.SetMenuBar(menubaredit)

        self.Centre()
        self.Show(1)

    def OnSaveFile(self, event):
        if self.last_name_saved:
            try:
                file = open(self.last_name_saved, 'w')
                text = self.text.GetValue()
                file.write(text)
                file.close()
                self.statusbar.SetStatusText(os.path.basename(self.last_name_saved) + ' saved', 0)
                self.modify = False
                self.statusbar.SetStatusText('', 1)

            except (IOError, error):
                dlg = wx.MessageDialog(self, 'Erro ao salvar arquivo\n' + str(error))
                dlg.ShowModal()
        else:
            self.OnSaveAsFile(event)

    def OnSaveAsFile(self, event):
         wcd='Todos os arquivos (*)|*|Arquivos do avaliador (*.rc)|*.rc|'
         dir = os.getcwd()
         save_dlg = wx.FileDialog(self, message='Salvar como...', defaultDir=dir, defaultFile='', wildcard=wcd, style=wx.SAVE | wx.OVERWRITE_PROMPT)
         if save_dlg.ShowModal() == wx.ID_OK:
             path = save_dlg.GetPath()
             try:
                 file = open(path, 'w')
                 text = self.text.GetValue()
                 file.write(text)
                 file.close()
                 self.last_name_saved = os.path.basename(path)
                 self.statusbar.SetStatusText(self.last_name_saved + ' saved', 0)
                 self.modify = False
                 self.statusbar.SetStatusText('', 1)

             except (IOError, error):
                 dlg = wx.MessageDialog(self, 'Erro ao salvar arquivo\n' + str(error))
                 dlg.ShowModal()
         save_dlg.Destroy()

    def QuitApplication(self, event):
        if self.modify:
            dlg = wx.MessageDialog(self, 'Salvar antes de sair?', '', wx.YES_NO | wx.YES_DEFAULT | wx.CANCEL | wx.ICON_QUESTION)
            val = dlg.ShowModal()
            if val == wx.ID_YES:
                self.OnSaveFile(event)
                if not self.modify:
                    wx.Exit()
            elif val == wx.ID_CANCEL:
                dlg.Destroy()
            else:
                self.Destroy()
        else:
            self.Destroy()
###
class MainWindow(wx.Frame):
        def __init__(self, parent, id, title):
          wx.Frame.__init__(self, parent, id, title, size=(800, 700))
###
          self.modify = False
          self.last_name_saved = ''
          self.replace = False
###
          self.CreateStatusBar()
###
          self.SetHelpText('')
###
          panel = wx.Panel(self, -1)
#		panel.SetBackgroundColour('WHITE')
###
#		menubar = wx.MenuBar()

#		Arquivo = wx.Menu()
#		Editar = wx.Menu()
#		Ferramentas = wx.Menu()
#		Ajuda = wx.Menu()

#		menubar.Append(Arquivo, '&Arquivo')
#		menubar.Append(Editar, '&Editar')
#		menubar.Append(Ferramentas, '&Ferramentas')
#		menubar.Append(Ajuda, 'A&juda')

#	        carregar = Arquivo.Append(wx.ID_OPEN, '&Carregar', 'Carregar um arquivo salvo/editado anteriormente')
#	        salvar = Arquivo.Append(wx.ID_SAVE, '&Salvar', 'Salvar as configurações em um arquivo separado')
#	        Arquivo.AppendSeparator()
#       	Sair = Arquivo.Append(wx.ID_EXIT, 'Sa&ir\tAlt-X', 'Fechar esta janela e sair do programa')

#		copiar = Editar.Append(wx.ID_COPY, 'C&opiar\tCtrl-C', 'Copiar texto')
#		colar = Editar.Append(wx.ID_PASTE, 'Co&lar\tCtrl-V', 'Colar texto')

#	        configs = Ferramentas.Append(wx.ID_PROPERTIES, 'Con&figurações...', 'Configura opções especiais do avaliador')
#       	terminal = Ferramentas.Append(-1, '&Terminal\tAlt-T', 'Abre uma janela do terminal')

#	        conteudo = Ajuda.Append(wx.ID_INDEX, '&Conteúdo\tF1', 'Mostrar ajuda do Avalidor')
#       	Ajuda.AppendSeparator()
#	      	ajonline = Ajuda.Append(wx.ID_HELP, 'Obter ajuda online...', 'Ir para o site do programa e obter ajuda online')
# 	    	problema = Ajuda.Append(-1, 'Reportar um problema', ' ')
#	    	Ajuda.AppendSeparator()
#	   	sobre = Ajuda.Append(wx.ID_ABOUT, '&Sobre', 'Mostrar informações sobre o Avaliador')

#		self.SetMenuBar(menubar)
###
          menubar = wx.MenuBar()

          arquivo = wx.Menu()
          novo = wx.MenuItem(arquivo, 101, '&Novo\tCtrl+N', 'Abre uma nova Janela')
#        	novo.SetBitmap(wx.Bitmap('icons/stock_new-16.png'))
#          arquivo.Append(novo)
          arquivo.Append(novo)

          abrir = wx.MenuItem(arquivo, 102, '&Abrir\tCtrl+A', 'Abre um arquivo existente')
#        	abrir.SetBitmap(wx.Bitmap('icons/stock_open-16.png'))
#          arquivo.Append(abrir)
          arquivo.Append(abrir)
          arquivo.AppendSeparator()

          salvar = wx.MenuItem(arquivo, 103, '&Salvar\tCtrl+S', 'Salvar arquivo')
#        	salvar.SetBitmap(wx.Bitmap('icons/stock_save-16.png'))
#          arquivo.Append(salvar)
          arquivo.Append(salvar)

          salvarcomo = wx.MenuItem(arquivo, 104, 'Salvar &Como...\tShift+Ctrl+S', 
    			'Salvar o arquivo com um nome diferente')
#        	salvarcomo.SetBitmap(wx.Bitmap('icons/stock_save_as-16.png'))
          arquivo.Append(salvarcomo)
          arquivo.AppendSeparator()

          sair = wx.MenuItem(arquivo, 105, 'Sa&ir\tCtrl+Q', 'Fechar o Avaliador')
#        	sair.SetBitmap(wx.Bitmap('icons/stock_exit-16.png'))
          arquivo.Append(sair)

          editar = wx.Menu()
          recortar = wx.MenuItem(editar, 106, '&Recortar\tCtrl+X', 'Recortar a Seleção')
#        	recortar.SetBitmap(wx.Bitmap('icons/stock_cut-16.png'))
          editar.Append(recortar)

          copiar = wx.MenuItem(editar, 107, '&Copiar\tCtrl+C', 'Copiar a Seleção')
#        	copiar.SetBitmap(wx.Bitmap('icons/stock_copy-16.png'))
          editar.Append(copiar)

          colar = wx.MenuItem(editar, 108, 'Co&lar\tCtrl+V', 'Colar Texto da Área de Transferência')
#	        colar.SetBitmap(wx.Bitmap('icons/stock_paste-16.png'))
          editar.Append(colar)

          deletar = wx.MenuItem(editar, 109, '&Deletar', 'Deletar Texto Selecionado')
#	        deletar.SetBitmap(wx.Bitmap('icons/stock_delete-16.png',))

          editar.Append(deletar)
          editar.AppendSeparator()
          editar.Append(110, 'Selecionar &Tudo\tCtrl+A', 'Selecionar Todo o Texto')

          visualizar = wx.Menu()
          visualizar.Append(111, '&Barra de Status', 'Mostar Barra de Status')

          ferramentas = wx.Menu()
          ferramentas.Append(112, 'Editar Manualmente\tCtrl+E', 'Abre um editor para configurar o arquivo manualmente')

          ajuda = wx.Menu()
          sobre = wx.MenuItem(ajuda, 113, '&Sobre\tF1', 'Sobre o Avaliador GDAD-CPTEC')
#	        sobre.SetBitmap(wx.Bitmap('icons/stock_about-16.png'))
          ajuda.Append(sobre)

          menubar.Append(arquivo, '&Arquivo')
#	        menubar.Append(editar, '&Editar')
#	        menubar.Append(visualizar, '&Visualizar')
          menubar.Append(ferramentas, '&Ferramentas')
          menubar.Append(ajuda, '&Ajuda')
          self.SetMenuBar(menubar)

          self.Bind(wx.EVT_MENU, self.NewApplication, id=101)
          self.Bind(wx.EVT_MENU, self.OnOpenFile, id=102)
          self.Bind(wx.EVT_MENU, self.OnSaveFile, id=103)
          self.Bind(wx.EVT_MENU, self.OnSaveAsFile, id=104)
          self.Bind(wx.EVT_MENU, self.QuitApplication, id=105)
          self.Bind(wx.EVT_MENU, self.OnCut, id=106)
          self.Bind(wx.EVT_MENU, self.OnCopy, id=107)
          self.Bind(wx.EVT_MENU, self.OnPaste, id=108)
          self.Bind(wx.EVT_MENU, self.OnDelete, id=109)
          self.Bind(wx.EVT_MENU, self.OnSelectAll, id=110)
          self.Bind(wx.EVT_MENU, self.ToggleStatusBar, id=111)
          self.Bind(wx.EVT_MENU, self.OpenEditor, id=112)
          self.Bind(wx.EVT_MENU, self.OnAbout, id=113)
###
          fs = self.GetFont().GetPointSize()
          bf = wx.Font(fs+4, wx.SWISS, wx.NORMAL, wx.BOLD)
          title = wx.StaticText(panel, -1, 'Avaliador GDAD-CPTEC v0.0.1a')
          title.SetFont(bf)

          help = wx.ContextHelpButton(panel)
          help.SetHelpText('wx.ContextHelpButton')
###
          line = wx.StaticLine(panel, -1)
###
          box1 = wx.Panel(panel, -1)
#		box1.SetBackgroundColour('WHITE')
          box1.SetHelpText('Marque uma opção para o tipo de modelo a ser avaliado.')
          s_box1 = wx.StaticBox(box1, -1, '1. Modelo')
          b1_box1 = wx.RadioButton(box1, -1, 'Regional', (10, 20), style=wx.RB_GROUP)
          rb2_box1 = wx.RadioButton(box1, -1, 'Global', (10, 40))
          rb2_box1 = wx.RadioButton(box1, -1, 'Ensemble', (10, 60))
          models = ['Eta 20Km', 'Eta 40Km', 'RPSAS 40Km']
          cb_box1 = wx.ComboBox(box1, -1, choices=models, pos=(115, 35))

          box2 = wx.Panel(panel, -1)
#		box2.SetBackgroundColour('WHITE')
          box2.SetHelpText('Indique as datas de início e fim da avaliação.')
          s_box2 = wx.StaticBox(box2, -1, '2. Datas')
          t1_box2 = wx.StaticText(box2, -1, 'Escolher Data Início:', (10, 25))
          t2_box2 = wx.StaticText(box2, -1, 'Escolher Data Fim:', (10, 55))
#          dp1_box2 = wx.DatePickerCtrl(box2, pos=(140, 22), size=(120,-1), style=wx.DP_DROPDOWN | wx.DP_SHOWCENTURY)
#          dp2_box2 = wx.DatePickerCtrl(box2, pos=(140, 52), size=(120,-1), style=wx.DP_DROPDOWN | wx.DP_SHOWCENTURY)
          
          box3 = wx.Panel(panel, -1)
#		box3.SetBackgroundColour('WHITE')
          box3.SetHelpText('Marque as variáveis a serem avaliadas.')
          s_box3 = wx.StaticBox(box3, -1, '3. Variáveis')
          cb1_box3 = wx.CheckBox(box3, -1, 'Temp. Virtual', (10, 20), style=wx.RB_GROUP)
          cb2_box3 = wx.CheckBox(box3, -1, 'PNMM', (10, 40))
          cb3_box3 = wx.CheckBox(box3, -1, 'Umid. Espec.', (10, 60))
          cb4_box3 = wx.CheckBox(box3, -1, 'Água Prec.', (140, 20))
          cb5_box3 = wx.CheckBox(box3, -1, 'Altura Geop.', (140, 40))
          cb6_box3 = wx.CheckBox(box3, -1, 'Vento Zonal', (140, 60))
          cb7_box3 = wx.CheckBox(box3, -1, 'Vento Meridional', (250, 20))
          rb1_box3 = wx.RadioButton(box3, -1, 'Todas', (250, 60), style=wx.RB_GROUP)

          box4 = wx.Panel(panel, -1)
#		box4.SetBackgroundColour('WHITE')
          box4.SetHelpText('Escolha os níveis desejados para a avaliação.')
          s_box4 = wx.StaticBox(box4, -1, '4. Níveis')
          rb1_box4 = wx.RadioButton(box4, -1, 'Todos os níveis do modelo (19)', (10, 20), style=wx.RB_GROUP)
          rb2_box4 = wx.RadioButton(box4, -1, 'Escolher:', (10, 40))
          rb3_box4 = wx.RadioButton(box4, -1, 'Outro(s):', (160, 40))
          tc1_box4 = wx.TextCtrl(box4, -1, pos=(240, 39), size=(80, 25))
          st1_box4 = wx.StaticText(box4, -1, 'hPa', pos=(322, 43))
          btn1_box4 = wx.Button(box4, -1, 'OK', pos=(350, 39), size=(30, 25))
          cb1_box4 = wx.CheckBox(box4, -1, '900 hPa', (10, 60))
          cb2_box4 = wx.CheckBox(box4, -1, '850 hPa', (90, 60))
          cb3_box4 = wx.CheckBox(box4, -1, '500 hPa', (180, 60))
          cb4_box4 = wx.CheckBox(box4, -1, '250 hPa', (270, 60))

          box5 = wx.Panel(panel, -1)
#		box5.SetBackgroundColour('WHITE')
          box5.SetHelpText('Escolha as métricas a serem calculadas.')
          s_box5 = wx.StaticBox(box5, -1, '5. Métricas')
          rb1_box5 = wx.RadioButton(box5, -1, 'Skill', (10, 10), style=wx.RB_GROUP)
          rb2_box5 = wx.RadioButton(box5, -1, 'Previsão X Obs.', (110, 10))
          rb3_box5 = wx.RadioButton(box5, -1, 'Previsão X Análise', (240, 10))
          rb4_box5 = wx.RadioButton(box5, -1, 'Precipitação', (240, 40))
          rb5_box5 = wx.RadioButton(box5, -1, 'Tudo', (240, 67))
          cb1_box5 = wx.CheckBox(box5, -1, 'Correlação', (10, 27))
          cb2_box5 = wx.CheckBox(box5, -1, 'EQM', (10, 47))
          cb3_box5 = wx.CheckBox(box5, -1, 'Viés', (10, 67))
          cb4_box5 = wx.CheckBox(box5, -1, 'Acerto', (110, 27))
          cb5_box5 = wx.CheckBox(box5, -1, 'Sup.est.', (110, 47))
          cb6_box5 = wx.CheckBox(box5, -1, 'Sub.est.', (110, 67))

          box6 = wx.Panel(panel, -1)
#		box6.SetBackgroundColour('WHITE')        	
          box6.SetHelpText('Indique os horários sinóticos das análises ou previsões.')
          s_box6 = wx.StaticBox(box6, -1, '6. Horários')
          cb1_box6 = wx.CheckBox(box6, -1, '00Z', (10, 20))
          cb2_box6 = wx.CheckBox(box6, -1, '06Z', (100, 20))
          cb3_box6 = wx.CheckBox(box6, -1, '12Z', (190, 20))
          cb4_box6 = wx.CheckBox(box6, -1, '18Z', (280, 20))
          cb5_box6 = wx.CheckBox(box6, -1, '00Z e 12Z', (10, 40))
          cb6_box6 = wx.CheckBox(box6, -1, '06Z e 18Z', (190, 40))
          rb1_box6 = wx.RadioButton(box6, -1, 'Todos', (10, 60), style=wx.RB_GROUP)
          
          box7 = wx.Panel(panel, -1)
#		box7.SetBackgroundColour('WHITE')
          box7.SetHelpText('Indique o tipo de produto, se análise ou previsão.')
          s_box7 = wx.StaticBox(box7, -1, '7. Tipo')
          rb1_box7 = wx.RadioButton(box7, -1, 'Análise', (10, 20), style=wx.RB_GROUP)
          rb2_box7 = wx.RadioButton(box7, -1, 'Previsão Curto Prazo (First Guess)', (10, 40))
          rb3_box7 = wx.RadioButton(box7, -1, 'Previsão Extendida', (10, 60))
          
          box8 = wx.Panel(panel, -1)
#		box8.SetBackgroundColour('WHITE')	
          box8.SetHelpText('Indique o intervalo entre uma análise e outra ou entre uma previsão e outra.')
          s_box8 = wx.StaticBox(box8, -1, '8. Incrementos')
          st1_box8 = wx.StaticText(box8, -1, 'Análises/Previsões:', pos=(10, 25))
          sc1_box8 = wx.SpinCtrl(box8, -1, '',  (142, 20), (60, -1))
          sc1_box8.SetRange(0, 168)
          sc1_box8.SetValue(3)
          st2_box8 = wx.StaticText(box8, -1, 'Tempo de Integração:', pos=(10, 55))
          sc2_box8 = wx.SpinCtrl(box8, -1, '',  (142, 50), (60, -1))
          sc2_box8.SetRange(0, 168)
          sc2_box8.SetValue(0)

          box9 = wx.Panel(panel, -1)
#		box9.SetBackgroundColour('WHITE')
          box9.SetHelpText('Aponte os diretórios onde estão os dados de análise ou previsão a serem avaliados.')
          s_box9 = wx.StaticBox(box9, -1, '9. Diretórios')
          st1_box9 = wx.StaticText(box9, -1, 'Entrada:', pos=(10, 24))
          tc1_box9 = wx.TextCtrl(box9, -1, pos=(67, 20), size=(180, 25))
          btn1_box9 = wx.Button(box9, -1, 'Escolher...', pos=(250, 20), size=(90, 25))
          st2_box9 = wx.StaticText(box9, -1, 'Saída:', pos=(10, 56))
          tc2_box9 = wx.TextCtrl(box9, -1, pos=(67, 52), size=(180, 25))
          btn2_box9 = wx.Button(box9, -1, 'Escolher...', pos=(250, 52), size=(90, 25))
          
          box10 = wx.Panel(panel, -1)
#		box10.SetBackgroundColour('WHITE')
          box10.SetHelpText('Indique o prefixo e o sufixo dos nomes dos arquivos de análise ou previsão.')
          s_box10 = wx.StaticBox(box10, -1, '10. Nomes')
          st1_box10 = wx.StaticText(box10, -1, 'Prefixo:', pos=(10, 24))
          tc1_box10 = wx.TextCtrl(box10, -1, pos=(67, 20), size=(180, 25))
          st2_box10 = wx.StaticText(box10, -1, 'Sufixo:', pos=(10, 56))
          tc2_box10 = wx.TextCtrl(box10, -1, pos=(67, 52), size=(180, 25))
          
          box11 = wx.Panel(panel, -1)
#		box11.SetBackgroundColour('WHITE')
          box11.SetHelpText('Escolha uma região para avaliação.')
          s_box11 = wx.StaticBox(box11, -1, '11. Região')
#		bmp_box11 = wx.Bitmap("mapa.png")
#		rb_box11 = wx.lib.mixins.rubberband.RubberBand(drawingSurface=box11,aspectRatio=None)
#		rb_box11.reset(aspectRatio=None)
          st1_box11 = wx.StaticText(box11, -1, 'Latitudes (lat1;lat2):', pos=(10, 24))
          tc1_box11 = wx.TextCtrl(box11, -1, pos=(153, 20), size=(100, 25))
          st2_box11 = wx.StaticText(box11, -1, 'Longitudes (lon1;lon2):', pos=(10, 56))
          tc2_box11 = wx.TextCtrl(box11, -1, pos=(153, 52), size=(100, 25))
          btn1_box11 = wx.Button(box11, -1, 'Escolher...', pos=(260, 34), size=(90, 25))
          
          box12 = wx.Panel(panel, -1)
#		box12.SetBackgroundColour('WHITE')
          box12.SetHelpText('Escolha uma climatologia para a avaliação.')
          s_box12 = wx.StaticBox(box12, -1, '12. Climatologia')
          rb1_box12 = wx.RadioButton(box12, -1, 'Sim', (10, 20), style=wx.RB_GROUP)
          rb2_box12 = wx.RadioButton(box12, -1, 'Não', (100, 20))
          st1_box12 = wx.StaticText(box12, -1, 'Escolher:', pos=(10, 56))
          tc1_box12 = wx.TextCtrl(box12, -1, pos=(67, 52), size=(180, 25))
          btn1_box12 = wx.Button(box12, -1, 'Abrir...', pos=(250, 52), size=(90, 25))

###
          btn1 = wx.Button(panel, -1, 'Abrir')
          btn2 = wx.Button(panel, -1, 'Salvar')
          btn3 = wx.Button(panel, -1, 'Recarregar')
          btn4 = wx.Button(panel, -1, 'Limpar')
          btn5 = wx.Button(panel, -1, 'Executar')
          btn6 = wx.Button(panel, -1, 'Fechar')
          ###	
          grid1 = wx.GridSizer(1, 2, 4, 4)
          grid1.Add(title, 0, wx.ALIGN_LEFT)
          grid1.Add(help, 0, wx.ALIGN_RIGHT)
          
          grid2 = wx.GridSizer(1, 1, 4, 4)
          grid2.Add(line, 0, wx.EXPAND)
          
          grid3 = wx.GridSizer(6, 2, 4, 4)
          grid3.Add(box1, 0, wx.EXPAND)
          grid3.Add(box2, 0, wx.EXPAND)
          grid3.Add(box3, 0, wx.EXPAND)
          grid3.Add(box4, 0, wx.EXPAND)
          grid3.Add(box5, 0, wx.EXPAND)
          grid3.Add(box6, 0, wx.EXPAND)
          grid3.Add(box7, 0, wx.EXPAND)
          grid3.Add(box8, 0, wx.EXPAND)
          grid3.Add(box9, 0, wx.EXPAND)
          grid3.Add(box10, 0, wx.EXPAND)
          grid3.Add(box11, 0, wx.EXPAND)
          grid3.Add(box12, 0, wx.EXPAND)

          grid4 = wx.GridSizer(1, 6, 4, 4)
          grid4.Add(btn1, 0, wx.EXPAND)
          grid4.Add(btn2, 0, wx.EXPAND)
          grid4.Add(btn3, 0, wx.EXPAND)
          grid4.Add(btn4, 0, wx.EXPAND)
          grid4.Add(btn5, 0, wx.EXPAND)
          grid4.Add(btn6, 0, wx.EXPAND)
          		
          vbox1 = wx.BoxSizer(wx.VERTICAL)
          hbox1 = wx.BoxSizer(wx.HORIZONTAL)
          wvbox1 = wx.BoxSizer(wx.VERTICAL)
          whbox1 = wx.BoxSizer(wx.HORIZONTAL)
          
          vbox2 = wx.BoxSizer(wx.VERTICAL)
          hbox2 = wx.BoxSizer(wx.HORIZONTAL)
          
          vbox3 = wx.BoxSizer(wx.VERTICAL)
          hbox3 = wx.BoxSizer(wx.HORIZONTAL)
          
          vbox4 = wx.BoxSizer(wx.VERTICAL)
          hbox4 = wx.BoxSizer(wx.HORIZONTAL)
          
          vbox5 = wx.BoxSizer(wx.VERTICAL)
          hbox5 = wx.BoxSizer(wx.HORIZONTAL)
          
          vbox6 = wx.BoxSizer(wx.VERTICAL)
          hbox6 = wx.BoxSizer(wx.HORIZONTAL)
          
          vbox7 = wx.BoxSizer(wx.VERTICAL)
          hbox7 = wx.BoxSizer(wx.HORIZONTAL)
          
          vbox8 = wx.BoxSizer(wx.VERTICAL)
          hbox8 = wx.BoxSizer(wx.HORIZONTAL)
          
          vbox9 = wx.BoxSizer(wx.VERTICAL)
          hbox9 = wx.BoxSizer(wx.HORIZONTAL)
          
          vbox10 = wx.BoxSizer(wx.VERTICAL)
          hbox10 = wx.BoxSizer(wx.HORIZONTAL)
          
          vbox11 = wx.BoxSizer(wx.VERTICAL)
          hbox11 = wx.BoxSizer(wx.HORIZONTAL)
          
          vbox12 = wx.BoxSizer(wx.VERTICAL)
          hbox12 = wx.BoxSizer(wx.HORIZONTAL)

          hbox1.Add(s_box1, 1, wx.EXPAND)
#		whbox1.Add(cb_box1, 0)
          hbox2.Add(s_box2, 1, wx.EXPAND)
          hbox3.Add(s_box3, 1, wx.EXPAND)
          hbox4.Add(s_box4, 1, wx.EXPAND)
          hbox5.Add(s_box5, 1, wx.EXPAND)
          hbox6.Add(s_box6, 1, wx.EXPAND)
          hbox7.Add(s_box7, 1, wx.EXPAND)
          hbox8.Add(s_box8, 1, wx.EXPAND)
          hbox9.Add(s_box9, 1, wx.EXPAND)
          hbox10.Add(s_box10, 1, wx.EXPAND)
          hbox11.Add(s_box11, 1, wx.EXPAND)
          hbox12.Add(s_box12, 1, wx.EXPAND)
          
          vbox1.Add(hbox1, 1, wx.EXPAND)
#		wvbox1.Add(whbox1, 0, wx.EXPAND | wx.ALIGN_LEFT)
#		box1.SetSizer(wvbox1)
          box1.SetSizer(vbox1)

          vbox2.Add(hbox2, 1, wx.EXPAND)
          box2.SetSizer(vbox2)
          
          vbox3.Add(hbox3, 1, wx.EXPAND)
          box3.SetSizer(vbox3)
          
          vbox4.Add(hbox4, 1, wx.EXPAND)
          box4.SetSizer(vbox4)
          
          vbox5.Add(hbox5, 1, wx.EXPAND)
          box5.SetSizer(vbox5)
          
          vbox6.Add(hbox6, 1, wx.EXPAND)
          box6.SetSizer(vbox6)
          
          vbox7.Add(hbox7, 1, wx.EXPAND)
          box7.SetSizer(vbox7)
          
          vbox8.Add(hbox8, 1, wx.EXPAND)
          box8.SetSizer(vbox8)
          
          vbox9.Add(hbox9, 1, wx.EXPAND)
          box9.SetSizer(vbox9)
          
          vbox10.Add(hbox10, 1, wx.EXPAND)
          box10.SetSizer(vbox10)
          
          vbox11.Add(hbox11, 1, wx.EXPAND)
          box11.SetSizer(vbox11)
          
          vbox12.Add(hbox12, 1, wx.EXPAND)
          box12.SetSizer(vbox12)
###
          sizer1 = wx.BoxSizer(wx.VERTICAL)
          sizer1.Add(grid1, 0, wx.EXPAND | wx.ALL, 5)
          sizer1.Add(grid2, 0, wx.EXPAND | wx.ALL, 5)
          sizer1.Add(grid3, 1, wx.EXPAND | wx.ALL, 5)
          sizer1.Add(grid4, 0, wx.ALIGN_CENTER | wx.ALL, 5)
          
          panel.SetSizerAndFit(sizer1)
###
          self.SetMinSize((750, 650))
          self.Centre()
          self.Show(True)
###

###
#        def __init__(self, parent):
#            wx.Panel.__init__(self, parent=parent)
#            self.SetBackgroundStyle(wx.BG_STYLE_CUSTOM)
#            self.frame = parent
 
#            sizer = wx.BoxSizer(wx.VERTICAL)
#            hSizer = wx.BoxSizer(wx.HORIZONTAL)
 
        def OnEraseBackground(self, evt):
          dc = evt.GetDC()
          if not dc:
                dc = wx.ClientDC(self)
                rect = self.GetUpdateRegion().GetBox()
                dc.SetClippingRect(rect)
                dc.Clear()
                bmp = wx.Bitmap('mapa.png')
                dc.DrawBitmap(bmp, 0, 0)
###
        def NewApplication(self, event):
            janelaaval = MainWindow(None, -1, 'Avaliador GDAD-CPTEC v0.0.1a (Nova Janela)')
            janelaaval.Centre()
            janelaaval.Show()

        def OpenEditor(self, event):
            janelaedit = Editor(None, -1, 'Editar Configurações Manualmente')
            janelaedit.Centre()
            janelaedit.Show()

        def OnOpenFile(self, event):
            file_name = os.path.basename(self.last_name_saved)
            if self.modify:
                dlg = wx.MessageDialog(self, 'Salvar modificações?', '', wx.YES_NO | wx.YES_DEFAULT | wx.CANCEL | wx.ICON_QUESTION)
                val = dlg.ShowModal()
                if val == wx.ID_YES:
                    self.OnSaveFile(event)
                    self.DoOpenFile()
                elif val == wx.ID_CANCEL:
                    dlg.Destroy()
                else:
                    self.DoOpenFile()
            else:
                self.DoOpenFile()

        def DoOpenFile(self):
            wcd = 'Todos os arquivos (*)|*|Arquivos do avaliador (*.rc)|*.rc|'
            dir = os.getcwd()
            open_dlg = wx.FileDialog(self, message='Escolha um arquivo', defaultDir=dir, defaultFile='', wildcard=wcd, style=wx.OPEN | wx.CHANGE_DIR)
            if open_dlg.ShowModal() == wx.ID_OK:
                path = open_dlg.GetPath()

                try:
                    file = open(path, 'r')
                    text = file.read()
                    file.close()
                    if self.text.GetLastPosition():
                        self.text.Clear()
                    self.text.WriteText(text)
                    self.last_name_saved = path
                    self.statusbar.SetStatusText('', 1)
                    self.modify = False

                except (IOError, error):
                    dlg = wx.MessageDialog(self, 'Erro ao abrir arquivo\n' + str(error))
                    dlg.ShowModal()

                except (UnicodeDecodeError, error):
                    dlg = wx.MessageDialog(self, 'Erro ao abrir arquivo\n' + str(error))
                    dlg.ShowModal()

            open_dlg.Destroy()

        def OnSaveFile(self, event):
            if self.last_name_saved:

                try:
                    file = open(self.last_name_saved, 'w')
                    text = self.text.GetValue()
                    file.write(text)
                    file.close()
                    self.statusbar.SetStatusText(os.path.basename(self.last_name_saved) + ' saved', 0)
                    self.modify = False
                    self.statusbar.SetStatusText('', 1)

                except (IOError, error):
                    dlg = wx.MessageDialog(self, 'Erro ao salvar arquivo\n' + str(error))
                    dlg.ShowModal()
            else:
                self.OnSaveAsFile(event)

        def OnSaveAsFile(self, event):
            wcd='Todos os arquivos (*)|*|Arquivos do avaliador (*.rc)|*.rc|'
            dir = os.getcwd()
            save_dlg = wx.FileDialog(self, message='Salvar como...', defaultDir=dir, defaultFile='', wildcard=wcd, style=wx.SAVE | wx.OVERWRITE_PROMPT)
            if save_dlg.ShowModal() == wx.ID_OK:
                path = save_dlg.GetPath()

                try:
                    file = open(path, 'w')
                    text = self.text.GetValue()
                    file.write(text)
                    file.close()
                    self.last_name_saved = os.path.basename(path)
                    self.statusbar.SetStatusText(self.last_name_saved + ' saved', 0)
                    self.modify = False
                    self.statusbar.SetStatusText('', 1)

                except (IOError, error):
                    dlg = wx.MessageDialog(self, 'Erro ao salvar arquivo\n' + str(error))
                    dlg.ShowModal()
            save_dlg.Destroy()

        def OnCut(self, event):
            self.text.Cut()

        def OnCopy(self, event):
            self.text.Copy()

        def OnPaste(self, event):
            self.text.Paste()

        def QuitApplication(self, event):
            if self.modify:
                dlg = wx.MessageDialog(self, 'Salvar antes de sair?', '', wx.YES_NO | wx.YES_DEFAULT | wx.CANCEL | wx.ICON_QUESTION)
                val = dlg.ShowModal()
                if val == wx.ID_YES:
                    self.OnSaveFile(event)
                    if not self.modify:
                        wx.Exit()
                elif val == wx.ID_CANCEL:
                    dlg.Destroy()
                else:
                    self.Destroy()
            else:
                self.Destroy()

        def OnDelete(self, event):
            frm, to = self.text.GetSelection()
            self.text.Remove(frm, to)

        def OnSelectAll(self, event):
            self.text.SelectAll()

        def OnTextChanged(self, event):
            self.modify = True
            self.statusbar.SetStatusText(' modified', 1)
            event.Skip()

        def OnKeyDown(self, event):
            keycode = event.GetKeyCode()
            if keycode == wx.WXK_INSERT:
                if not self.replace:
                    self.statusbar.SetStatusText('INS', 2)
                    self.replace = True
                else:
                    self.statusbar.SetStatusText('', 2)
                    self.replace = False
                event.Skip()

        def ToggleStatusBar(self, event):
            if self.statusbar.IsShown():
                self.statusbar.Hide()
            else:
                self.statusbar.Show()

        def StatusBar(self):
            self.statusbar = self.CreateStatusBar()
            self.statusbar.SetFieldsCount(3)
            self.statusbar.SetStatusWidths([-5, -2, -1])

        def OnAbout(self, event):
            dlg = wx.MessageDialog(self, 'Avaliador GDAD-CPTEC v0.0.1a\n\nO Avaliador GDAD-CPTEC é um software de avaliação de modelos numéricos escrito em linguagem FORTRAN.\n\nCopyright 2010 Os autores de Avaliador GDAD-CPTEC.\n\nPara maiores informações sobre este programa,\nvisite:\n\nhttp://assimila.cptec.inpe.br\n\nEsta interface está rodando nas versões 2.8.10.1 do wxPython e 2.6.4 do Python.', 'Sobre Avaliador GDAD-CPTEC', wx.OK | wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
###
app = wx.App()
MainWindow(None, -1, 'Avaliador GDAD-CPTEC v0.0.1a')
app.MainLoop()
