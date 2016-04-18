/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package br.scamtec.frame;

import br.scamtec.classes.GraficoComparativo;
import br.scamtec.classes.Recorte;
import br.scamtec.classes.UtilData;
import java.awt.Color;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.print.attribute.standard.MediaSize;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.table.DefaultTableModel;
import java.io.FileWriter;
import java.nio.channels.FileChannel;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.NumberTickUnit;
import org.jfree.chart.plot.Marker;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.ValueMarker;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 *
 * @author paulo.henrique
 */
public class ScamtecConfiguracao extends javax.swing.JInternalFrame {

    int posicaoDoRecorteNaLista;
    int contExp = 0;
    int useClima = 1;
    int usePrecip = 0;
    String endArq = "";
    List<String> nomeExp = new ArrayList<>();
    int id_linhaTabelaExp = -1; //id da linha da tabela de experimento
    //Grafico ------------------------------------------
    //String linha = "";
    List<String> umaLinha = new ArrayList<>();
    String[] cabecalho = null;
    String[] val = null;
    String[][][] valores;
    int qauntExp = 1;
    private XYSeriesCollection dataset;
    //---------------------------------------------------
    /*
     *  Autor:Rafael
     *  Funcionalidade: Váriaveis e métodos para fazer
     *                  várias rodadas no SCAMTEC
     */
    public List<Recorte> listaRecorte = new ArrayList<Recorte>();
    public String line2 = "";
    public int indiceDaListaDeRecortes = -1;

    public static void CopiarArquivo(File source, File destination) throws IOException {
        if (destination.exists()) {
            destination.delete();
        }

        FileChannel sourceChannel = null;
        FileChannel destinationChannel = null;

        try {
            sourceChannel = new FileInputStream(source).getChannel();
            destinationChannel = new FileOutputStream(destination).getChannel();
            sourceChannel.transferTo(0, sourceChannel.size(),
                    destinationChannel);
        } finally {
            if (sourceChannel != null && sourceChannel.isOpen()) {
                sourceChannel.close();
            }
            if (destinationChannel != null && destinationChannel.isOpen()) {
                destinationChannel.close();
            }
        }
    }

    public void ExecutarScamtec(int posicaoDaLista) {
        DefaultTableModel modelo = (DefaultTableModel) tabelaEXP.getModel();
        int j = posicaoDaLista;
        if (indiceDaListaDeRecortes >= 0) {

            if (jDateDataIni.getDate() == null) {
                JOptionPane.showMessageDialog(rootPane, "INFORME A DATA INICIAL");
                jTabbedPaneGrafico.setSelectedIndex(0);
                jDateDataIni.requestFocus();
            } else if (jDateDataFinal.getDate() == null) {
                JOptionPane.showMessageDialog(rootPane, "INFORME A DATA FINAL");
                jTabbedPaneGrafico.setSelectedIndex(0);
                jDateDataFinal.requestFocus();
            } else if (txtEndAnalise.getText().equals("")) {
                JOptionPane.showMessageDialog(rootPane, "INFORME O ENDEREÇO DO ARQUIVO DE ANALISE");
                jTabbedPaneGrafico.setSelectedIndex(1);
                txtEndAnalise.requestFocus();
            } else if (txtQauntExp.getText().equals("")) {
                JOptionPane.showMessageDialog(rootPane, "INFORME A QUANTIDADE DE EXPERIMENTO");
                jTabbedPaneGrafico.setSelectedIndex(1);
                txtQauntExp.requestFocus();
            } else if (modelo.getRowCount() == 0) {
                JOptionPane.showMessageDialog(rootPane, "INFORME O ARQUIVO DE EXPERIMENTO");
                jTabbedPaneGrafico.setSelectedIndex(1);
                txtEndExp.requestFocus();
            } else if (jRadioButtonUseClima.isSelected() == true && txtEndClima.getText().equals("")) {
                JOptionPane.showMessageDialog(rootPane, "INFORME O ARQUIVO DE CLIMATOLOGIA");
                jTabbedPaneGrafico.setSelectedIndex(2);
                txtEndClima.requestFocus();
            } else if (jRadioButtonUsePrecip.isSelected() == true && txtEndPrecip.getText().equals("")) {
                JOptionPane.showMessageDialog(rootPane, "INFORME O ARQUIVO DE PRECIPITAÇÃO");
                jTabbedPaneGrafico.setSelectedIndex(2);
                txtEndPrecip.requestFocus();
            } else if (txtEndSaida.getText().equals("")) {
                JOptionPane.showMessageDialog(rootPane, "INFORME O ENDEREÇO DE SAÍDA");
                jTabbedPaneGrafico.setSelectedIndex(2);
                txtEndSaida.requestFocus();
            } else if (txtEndExecutavel.getText().equals("")) {
                JOptionPane.showMessageDialog(rootPane, "INFORME O ENDEREÇO DO EXECUTÁVEL");
                jTabbedPaneGrafico.setSelectedIndex(2);
                txtEndExecutavel.requestFocus();
            } else {

                try {


                    File arquivo;

                    arquivo = new File("scamtec.conf");
                    FileOutputStream fos = new FileOutputStream(arquivo);
                    String texto = "#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!"
                            + "\n#                  SCAMTeC - GDAD/CPTEC/INPE - 2010                   !"
                            + "\n#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!"
                            + "\n#======================================================================"
                            + "\n#                          Runtime options"
                            + "\n#";
                    // Configuracao do tempo
                    fos.write(texto.getBytes());
                    //Date dataI = jDateDataIni.getDate();
                    //String dataIni = UtilData.dateToStringSemBarra(dataI) + jComboBoxHoraIni.getSelectedItem();
                    //texto = "\nStarting Time: " + dataIni + " #Format  :: YYYYMMDDHH";
                    texto = "\nStarting Time: " + listaRecorte.get(j).getDataInicial() + listaRecorte.get(j).getHoraInicial() + " #Format  :: YYYYMMDDHH";
                    fos.write(texto.getBytes());

                    //Date dataF = jDateDataFinal.getDate();
                    //String dataFinal = UtilData.dateToStringSemBarra(dataF) + jComboBoxHoraFinal.getSelectedItem();
                    //texto = "\nEnding Time: " + dataFinal + " #Format  :: YYYYMMDDHH";
                    texto = "\nEnding Time: " + listaRecorte.get(j).getDataFinal() + listaRecorte.get(j).getHoraFinal() + " #Format  :: YYYYMMDDHH";
                    fos.write(texto.getBytes());

                    //texto = "\nAnalisys Time Step: " + txtAnaliseTempo.getText() + " #Format  :: HH";
                    texto = "\nAnalisys Time Step: " + listaRecorte.get(j).getAnaliseTempo() + " #Format  :: HH";
                    fos.write(texto.getBytes());

                    //texto = "\nForecast Time Step: " + txtPrevisaoTempo.getText() + " #Format  :: HH";
                    texto = "\nForecast Time Step: " + listaRecorte.get(j).getPrevisaoTempo() + " #Format  :: HH";
                    fos.write(texto.getBytes());

                    //texto = "\nForecast Total Time: " + txtPrevisaoTempoTotal.getText() + " #Format  :: HH";
                    texto = "\nForecast Total Time: " + listaRecorte.get(j).getPrevisaoTempoTotal() + " #Format  :: HH";
                    fos.write(texto.getBytes());

                    //texto = "\nHistory Time: " + txtHistoriaTempo.getText() + " #Format  :: HH";
                    texto = "\nHistory Time: " + listaRecorte.get(j).getHistoriaTempo() + " #Format  :: HH";
                    fos.write(texto.getBytes());

                    texto = "\n\n#======================================================================"
                            + "\n#                       DOMAIN SPECIFICATION"
                            + "\n# Definition of Running Domain"
                            + "\n# Specify the domain extremes in latitude and longitude"
                            + "\n#"
                            + "\n#              +----------------------------------+"
                            + "\n#              |**********************************|"
                            + "\n#              |**********************************|"
                            + "\n#            L |*************** +--------------+ *|"
                            + "\n#            A |*************** |              | *|"
                            + "\n#            T |*************** |     Area     | *|"
                            + "\n#            I | * +--------+ * |      02      | *|"
                            + "\n#            T | * |        | * |              | *|"
                            + "\n#            U | * |  area  | * |              | *|"
                            + "\n#            D | * |   01   | * |              | *|"
                            + "\n#            E | * |        | * +--------------+ *|"
                            + "\n#              | * |        | ********************|"
                            + "\n#              | * +--------+ ********************|"
                            + "\n#              | *********************************|"
                            + "\n#              +----------------------------------+"
                            + "\n#                        L O N G I T U D E"
                            + "\n\nrun domain number: 1 # Number of areas to analise "
                            + "\n\n# domain of each area"
                            + "\n#                    AREAS     1               2            3        4          5"
                            + "\n#                 	1                 America Sul             Brasil   hemisferio sul ";
                    fos.write(texto.getBytes());
                    //configuracao do dominio
                    //texto = "\nrun domain lower left lat: " + txtLatEsq.getText();
                    texto = "\nrun domain lower left lat: " + listaRecorte.get(j).getLatEsq();
                    fos.write(texto.getBytes());
                    //texto = "\nrun domain lower left lon: " + txtLongInf.getText();
                    texto = "\nrun domain lower left lon: " + listaRecorte.get(j).getLongInf();
                    fos.write(texto.getBytes());
                    //texto = "\nrun domain upper right lat: " + txtLatDireta.getText();
                    texto = "\nrun domain upper right lat: " + listaRecorte.get(j).getLatDireta();
                    fos.write(texto.getBytes());
                    //texto = "\nrun domain upper right lon: " + txtLongSup.getText();
                    texto = "\nrun domain upper right lon: " + listaRecorte.get(j).getLongSup();
                    fos.write(texto.getBytes());
                    //texto = "\nrun domain resolution dx: " + txtResolucaoX.getText();
                    texto = "\nrun domain resolution dx: " + listaRecorte.get(j).getResolucaoX();
                    fos.write(texto.getBytes());
                    //texto = "\nrun domain resolution dy: " + txtResolucaoY.getText();
                    texto = "\nrun domain resolution dy: " + listaRecorte.get(j).getResolucaoY();
                    fos.write(texto.getBytes());

                    texto = "\n\n#======================================================================"
                            + "\n#                              Files to Analyse"
                            + "\n#"
                            + "\n#======================================"
                            + "\n# Reference File"
                            + "\n#"
                            + "\n#         Name diretory File_Name_with_mask"
                            + "\n#";
                    fos.write(texto.getBytes());

                    // arquivo de analise
                    int refModel = jComboBoxRefModelAnlise.getSelectedIndex() + 1;
                    texto = "\nReference model: " + refModel;
                    fos.write(texto.getBytes());
                    texto = "\nReference file: " + txtEndAnalise.getText();
                    fos.write(texto.getBytes());
                    texto = "\nReference label: REFER";
                    fos.write(texto.getBytes());

                    texto = "\n\n#======================================"
                            + "\n# Experiment Files"
                            + "\n#\n";
                    fos.write(texto.getBytes());

                    //arquivos de experimento
                    texto = "\nNumber of Experiments: " + txtQauntExp.getText();
                    fos.write(texto.getBytes());
                    texto = "\n\nExperiments:";
                    fos.write(texto.getBytes());
                    texto = "\n#ModelId Name Diretory File_Name_with_mask";
                    fos.write(texto.getBytes());
                    int quantExp = Integer.parseInt(txtQauntExp.getText());
                    for (int i = 1; i < quantExp + 1; i++) {

                        texto = "\n" + modelo.getValueAt(i - 1, 0).toString();
                        fos.write(texto.getBytes());
                        texto = " " + modelo.getValueAt(i - 1, 2).toString();
                        fos.write(texto.getBytes());
                        texto = " " + modelo.getValueAt(i - 1, 3).toString();
                        fos.write(texto.getBytes());
                        nomeExp.add(modelo.getValueAt(i - 1, 4).toString());
                        texto = " # " + modelo.getValueAt(i - 1, 4).toString();
                        fos.write(texto.getBytes());
                    }
                    texto = "\n::";
                    fos.write(texto.getBytes());
                    texto = "\n\n#======================================"
                            + "\n# Climatology File"
                            + "\n#\n";
                    fos.write(texto.getBytes());

                    //Arquivo de climatologia
                    texto = "\nUse Climatology: " + useClima + " # 0-do not use, 1-use";
                    fos.write(texto.getBytes());
                    texto = "\n# Diretory prefix mask sulfixk";
                    fos.write(texto.getBytes());
                    texto = "\nClimatology Model Id: 3";
                    fos.write(texto.getBytes());
                    texto = "\nClimatology file: " + txtEndClima.getText();
                    fos.write(texto.getBytes());

                    texto = "\n\n#======================================"
                            + "\n# Precipitation File"
                            + "\n#\n";
                    fos.write(texto.getBytes());

                    //Arquivo de Precipitacao
                    texto = "\nUse Precipitation: " + usePrecip + " # 0-do not use, 1-use";
                    fos.write(texto.getBytes());
                    texto = "\n# Diretory prefix mask sulfixk";
                    fos.write(texto.getBytes());
                    texto = "\nPrecipitation Model Id: 5";
                    fos.write(texto.getBytes());
                    texto = "\nPrecipitation file: " + txtEndPrecip.getText();
                    fos.write(texto.getBytes());

                    texto = "\nDefine o Range do Histograma: " + txtPrecipRange.getText() + "                                         # exemplo: 2";
                    fos.write(texto.getBytes());
                    texto = "\nDefine valor do limite inferior da ultima classe do histograma: " + txtPrecipLimite.getText() + "      # exemplo: 100";
                    fos.write(texto.getBytes());
                    texto = "\nDefine valor do minimo inferior da primeira classe do histograma: " + txtPrecipMinimo.getText() + "     # exemplo: 0";
                    fos.write(texto.getBytes());
                    texto = "\nDefine qual Precipitacao deseja avaliar: " + txtPrecipTipo.getText() + "                             # exemplo: 16 para TOTAL ou 17 para CONVECTIVE";
                    fos.write(texto.getBytes());
                    texto = "\nDefine o periodo de acumulo de precpitacao da observacao: " + txtPrecipAcumuloObs.getText() + "             # exemplo: 3 ";
                    fos.write(texto.getBytes());
                    texto = "\nDefine o periodo de acumulo de precpitacao do experimento: " + txtPrecipAcumuloExp.getText() + "           # exemplo: 24";
                    fos.write(texto.getBytes());

                    texto = "\n\n#======================================"
                            + "\n# Calculate EOFs"
                            + "\n#";
                    fos.write(texto.getBytes());
                    //Selecionar EOF Atualmente desligada
                    texto = "\nUse EOFs: 0	# 0-do not use, 1-use"
                            + "\nDefine a quantidade de EOFs: 4           # exemplo: 4";
                    fos.write(texto.getBytes());

                    //Arquivo de Saida
                    texto = "\n\n#======================================================================"
                            + "\n# OUTPUT"
                            + "\n#";
                    fos.write(texto.getBytes());
                    //texto = "\n\nOutput directory: " + txtEndSaida.getText();//código original
                    texto = "\n\nOutput directory: " + txtEndSaida.getText() + Recorte.ReplaceEspacoPorAnderline(listaRecorte.get(j).getRegiao()) + "/";//Código modificado
                    File dir = new File(txtEndSaida.getText() + Recorte.ReplaceEspacoPorAnderline(listaRecorte.get(j).getRegiao()));
                    if (dir.mkdir()) {
                    } else {
                        System.out.println("Erro ao criar diretório, o diretório ja existia, arquivos serão sobrescritos");
                    }
                    Main.endSaida = txtEndSaida.getText() + Recorte.ReplaceEspacoPorAnderline(listaRecorte.get(j).getRegiao()) + "/";
                    fos.write(texto.getBytes());

                    fos.close();

                    File source = new File("scamtec.conf");
                    File destination = new File(txtEndSaida.getText() + Recorte.ReplaceEspacoPorAnderline(listaRecorte.get(j).getRegiao()) + "/scamtec.conf");


                    CopiarArquivo(source, destination);

                    // Lendo do arquivo
                    arquivo = new File("scamtec.conf");
                    FileInputStream fis = new FileInputStream(arquivo);

                    int ln;
                    while ((ln = fis.read()) != -1) {
                        System.out.print((char) ln);
                    }

                    fis.close();
                    System.out.printf("\n");



                    //new Thread(new Carregar()).start();
                    ExecutaScamtec exe = new ExecutaScamtec();
                    Thread theradExe = new Thread(exe);
                    theradExe.start();
                    JOptionPane.showMessageDialog(rootPane, "CONFIGURACAO REALIZADA COM SUCESSO");



                } catch (Exception ee) {
                    // ee.printStackTrace();
                    JOptionPane.showMessageDialog(rootPane, "ocorreu um erro: " + ee);
                }

            }

        } else {
            JOptionPane.showMessageDialog(rootPane, "Todos os processos foram finalizados");
        }
    }

    //---------------------------------------------------
    /**
     * Creates new form teste
     */
    public ScamtecConfiguracao() {
        initComponents();
        jComboBoxRegiaoActionPerformed(null);
        tabelaPadrao();
        jRadioButtonUseClima.setSelected(true);
        parametrosPrecip();
        txtResult.setText("");

        //Grafico------------------------------
        jRadioButtonAcor.setSelected(true);

        //--------------------------------------

    }

    private void tabelaPadrao() {

        DefaultTableModel modelo = ((DefaultTableModel) tabelaEXP.getModel());
        modelo.setRowCount(0);
        modelo.setColumnCount(0);

        modelo.addColumn("ID MODELO");
        modelo.addColumn("MODELO");
        modelo.addColumn("EXP");
        modelo.addColumn("ENDEREÇO");
        modelo.addColumn("NOME");

        tabelaEXP.getColumnModel().getColumn(0).setPreferredWidth(10);
        tabelaEXP.getColumnModel().getColumn(1).setPreferredWidth(20);
        tabelaEXP.getColumnModel().getColumn(2).setPreferredWidth(10);
        tabelaEXP.getColumnModel().getColumn(3).setPreferredWidth(800);
        tabelaEXP.getColumnModel().getColumn(4).setPreferredWidth(200);
    }

    private void parametrosPrecip() {
        txtPrecipAcumuloExp.setText("24");
        txtPrecipAcumuloObs.setText("3");
        txtPrecipLimite.setText("70");
        txtPrecipMinimo.setText("0");
        txtPrecipRange.setText("2");
        txtPrecipTipo.setText("16");

        //parametro do tempo
        txtHistoriaTempo.setText("48");
        txtAnaliseTempo.setText("06");
        txtPrevisaoTempo.setText("06");
        txtPrevisaoTempoTotal.setText("72");

    }

    public class ExecutaScamtec implements Runnable {

        @Override
        public void run() {
            try {


                String line;
                line2 += "";
                txtResult.setText(line2);

                Process p = Runtime.getRuntime().exec(txtEndExecutavel.getText() + "scamtec.x");

                try (BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()))) {
                    int cont = 0;

                    //setando o tamanho total da barra
                    barraProcesso.setMaximum(difDatas());
                    //barraProcesso.setIndeterminate(true);
                    // variavel para verificar se é commentario
                    Character inicio = '!';

                    while ((line = input.readLine()) != null) {
                        //variavel que é escriva no jtextarea
                        line2 = line2 + line + "\n";

                        //variavel para pegar uma letra da linha
                        Character comentario = line.charAt(1);

                        //verifica se é comentario para nao somar na barra de progresso
                        if (comentario.equals(inicio)) {
                            //System.out.println("nao faz nada \n");
                            //nao faz nada   
                        } else {
                            cont = cont + 1;
                            barraProcesso.setValue(cont);
                            // barraProcesso.setIndeterminate(true);
                        }
                        //escrevendo no jtextarea
                        txtResult.setText(line2);
                        txtResult.selectAll();
                        //System.out.println(line);
                    }
                    line2 += "\n\n ----------Fim do Recorte---------- \n\n";
                    txtResult.setText(line2);
                    txtResult.selectAll();
                    indiceDaListaDeRecortes--;
                    ExecutarScamtec(indiceDaListaDeRecortes);
                    //barraProcesso.setIndeterminate(false);
                }

            } catch (Exception err) {
                err.printStackTrace();
            }
            leituraArqSaida();

        }
    }

    private int difDatas() {

        Date dataA = UtilData.getDataCalendario(jDateDataIni.getDate());
        Date dataVenc = UtilData.getDataCalendario(jDateDataFinal.getDate());

        int horaPrev = Integer.parseInt(txtPrevisaoTempo.getText());
        int horaAna = Integer.parseInt(txtAnaliseTempo.getText());

        int horaIni = Integer.parseInt("" + jComboBoxHoraIni.getSelectedItem());
        int horaFinal = Integer.parseInt("" + jComboBoxHoraFinal.getSelectedItem());
        int difHora = ((horaFinal + horaAna) - horaIni);

        difHora = difHora / horaPrev;
        //System.out.println("\nHELLO3 " + difHora);
        int horaPrevTotal = Integer.parseInt(txtPrevisaoTempoTotal.getText());

        int quantPrev = (horaPrevTotal / horaPrev) + 1;

        int difData = dataDiff(dataA, dataVenc);

        int tempo = 24 / horaAna;

        int total = 0;

        total = (difData * (tempo) * quantPrev) + quantPrev * difHora;
        //System.out.println("\nRESULT " + difData + " " + tempo + " " + quantPrev + quantPrev + " " + difHora + " " + total);
        return total;
    }

    public static int dataDiff(java.util.Date dataLow, java.util.Date dataHigh) {

        GregorianCalendar startTime = new GregorianCalendar();
        GregorianCalendar endTime = new GregorianCalendar();

        GregorianCalendar curTime = new GregorianCalendar();
        GregorianCalendar baseTime = new GregorianCalendar();

        startTime.setTime(dataLow);
        endTime.setTime(dataHigh);

        int dif_multiplier = 1;

        // Verifica a ordem de inicio das datas  
        if (dataLow.compareTo(dataHigh) < 0) {
            baseTime.setTime(dataHigh);
            curTime.setTime(dataLow);
            dif_multiplier = 1;
        } else {
            baseTime.setTime(dataLow);
            curTime.setTime(dataHigh);
            dif_multiplier = -1;
        }

        int result_years = 0;
        int result_months = 0;
        int result_days = 0;

        // Para cada mes e ano, vai de mes em mes pegar o ultimo dia para import acumulando  
        // no total de dias. Ja leva em consideracao ano bissesto  
        while (curTime.get(GregorianCalendar.YEAR) < baseTime.get(GregorianCalendar.YEAR)
                || curTime.get(GregorianCalendar.MONTH) < baseTime.get(GregorianCalendar.MONTH)) {

            int max_day = curTime.getActualMaximum(GregorianCalendar.DAY_OF_MONTH);
            result_months += max_day;
            curTime.add(GregorianCalendar.MONTH, 1);

        }
        // Marca que é um saldo negativo ou positivo  
        result_months = result_months * dif_multiplier;


        // Retirna a diferenca de dias do total dos meses  
        result_days += (endTime.get(GregorianCalendar.DAY_OF_MONTH) - startTime.get(GregorianCalendar.DAY_OF_MONTH));

        return result_years + result_months + result_days;
    }

    //metodo para mostrar a barra de progresso que salva os graficos
    public class Carregar implements Runnable {

        @Override
        public void run() {
            BarraProgressoSalvaGraficos barra = new BarraProgressoSalvaGraficos();
            barra.setVisible(true);
            for (int i = 0; i <= 100; i++) {
                barra.barraGraficos.setValue(i);
                try {
                    Thread.sleep(150);

                } catch (InterruptedException ex) {
                    Logger.getLogger(ScamtecConfiguracao.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
            barra.dispose();
            JOptionPane.showMessageDialog(rootPane, "TODAS OS GRÁFICOS GRAVADOS COM SUCESSO!!!");
        }
    }

    //Metodo para procurar palavras
    public void procura(String arq) {
        String NomeArq = arq;

        //String pal = "Starting Time:";

        //Starting Time:
        try {

            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Starting Time:")) {
                    vetor = linha.split(" ");
                    // System.out.println(linha);
                }
            }
            String hora = "";
            Date dataIni;
            dataIni = UtilData.stringToDateSemBarra(vetor[2]);

            for (int i = 8; i < 10; i++) {
                Character charc = vetor[2].charAt(i);
                hora = hora + charc;
            }
            jDateDataIni.setDate(dataIni);
            jComboBoxHoraIni.setSelectedItem(hora);


        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Ending Time:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Ending Time:")) {
                    vetor = linha.split(" ");
                    //  System.out.println(linha);
                }
            }
            String hora = "";
            Date dataIni;
            dataIni = UtilData.stringToDateSemBarra(vetor[2]);

            for (int i = 8; i < 10; i++) {
                Character charc = vetor[2].charAt(i);
                hora = hora + charc;
            }
            jDateDataFinal.setDate(dataIni);
            jComboBoxHoraFinal.setSelectedItem(hora);


        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Analisys Time Step:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Analisys Time Step:")) {
                    vetor = linha.split(" ");
                    // System.out.println(linha);
                }
            }
            txtAnaliseTempo.setText(vetor[3]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Forecast Time Step:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Forecast Time Step:")) {
                    vetor = linha.split(" ");
                    // System.out.println(linha);
                }
            }
            txtPrevisaoTempo.setText(vetor[3]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Forecast Total Time:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Forecast Total Time:")) {
                    vetor = linha.split(" ");
                    // System.out.println(linha);
                }
            }
            txtPrevisaoTempoTotal.setText(vetor[3]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //History Time:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("History Time:")) {
                    vetor = linha.split(" ");
                    //  System.out.println(linha);
                }
            }
            txtHistoriaTempo.setText(vetor[2]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //run domain lower left lat:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("run domain lower left lat:")) {
                    vetor = linha.split(" ");
                    //  System.out.println(linha);
                }
            }
            txtLatEsq.setText(vetor[5]);
            if (vetor[5].equals("-49.875")) {
                jComboBoxRegiao.setSelectedIndex(0);
            } else if (vetor[5].equals("-80")) {
                jComboBoxRegiao.setSelectedIndex(1);
            } else if (vetor[5].equals("-35")) {
                jComboBoxRegiao.setSelectedIndex(2);
            } else if (vetor[5].equals("-27")) {
                jComboBoxRegiao.setSelectedIndex(3);
            } else {
                jComboBoxRegiao.setSelectedIndex(4);
            }

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //run domain lower left lon:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("run domain lower left lon:")) {
                    vetor = linha.split(" ");
                    // System.out.println(linha);
                }
            }
            txtLongInf.setText(vetor[5]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //run domain upper right lat:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("run domain upper right lat:")) {
                    vetor = linha.split(" ");
                    // System.out.println(linha);
                }
            }
            txtLatDireta.setText(vetor[5]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //run domain upper right lon:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("run domain upper right lon:")) {
                    vetor = linha.split(" ");
                    // System.out.println(linha);
                }
            }
            txtLongSup.setText(vetor[5]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //run domain resolution dx:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("run domain resolution dx:")) {
                    vetor = linha.split(" ");
                    // System.out.println(linha);
                }
            }
            txtResolucaoX.setText(vetor[4]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //run domain resolution dy:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("run domain resolution dy:")) {
                    vetor = linha.split(" ");
                    //System.out.println(linha);
                }
            }
            txtResolucaoY.setText(vetor[4]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Reference model:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Reference model:")) {
                    vetor = linha.split(" ");
                    //  System.out.println(linha);
                }
            }
            int index = Integer.parseInt(vetor[2]) - 1;
            jComboBoxRefModelAnlise.setSelectedIndex(index);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Reference file:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Reference file:")) {
                    vetor = linha.split(" ");
                    // System.out.println(linha);
                }
            }
            txtEndAnalise.setText(vetor[2]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Number of Experiments:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Number of Experiments:")) {
                    vetor = linha.split(" ");
                    //System.out.println(linha);
                }
            }
            txtQauntExp.setText(vetor[3]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Experiments:
        int nunExp = Integer.parseInt(txtQauntExp.getText());
        for (int i = 1; i <= nunExp; i++) {
            try {
                String linha = "";
                String[] vetor = null;
                String[] nomeEx = null;
                BufferedReader in = new BufferedReader(new FileReader(arq));
                while ((linha = in.readLine()) != null) {
                    if (linha.contains("EXP0" + i)) {
                        vetor = linha.split(" ");
                        nomeEx = linha.split("#");
                        //System.out.println(vetor[i]);

                    }
                }

                DefaultTableModel modelo = ((DefaultTableModel) tabelaEXP.getModel());

                int contLinhaTabela = modelo.getRowCount() + 1;
                if (contLinhaTabela > Integer.parseInt(txtQauntExp.getText())) {
                    JOptionPane.showMessageDialog(rootPane, "QUANTIDADE NÃO PERMITIDA!!!");
                    txtEndExp.setText("");
                } else {
                    String[] dados = new String[13];
                    dados[0] = vetor[0];
                    jComboBoxRefModelExp.setSelectedIndex(Integer.parseInt(vetor[0]) - 1);
                    dados[1] = "" + jComboBoxRefModelExp.getSelectedItem();
                    dados[2] = vetor[1];
                    dados[3] = vetor[2];
                    dados[4] = nomeEx[1];
                    modelo.addRow(dados);

                }

            } catch (Exception e) {
                System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
            }
        }

        //Use Climatology:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Use Climatology:")) {
                    vetor = linha.split(" ");
                    //System.out.println(linha);                    
                }
            }
            if (vetor[2].equals("0")) {
                jRadioButtonUseClima.setSelected(false);
            } else {
                jRadioButtonUseClima.setSelected(true);
            }

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Climatology file:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Climatology file:")) {
                    vetor = linha.split(" ");
                    //System.out.println(linha);                    
                }
            }
            txtEndClima.setText(vetor[2]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Use Precipitation:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Use Precipitation:")) {
                    vetor = linha.split(" ");
                    //System.out.println(linha);                    
                }
            }
            if (vetor[2].equals("0")) {
                jRadioButtonUsePrecip.setSelected(false);
            } else {
                jRadioButtonUsePrecip.setSelected(true);
            }

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Precipitation file: 
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Precipitation file:")) {
                    vetor = linha.split(" ");
                    //System.out.println(linha);                    
                }
            }
            txtEndPrecip.setText(vetor[2]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Define o Range do Histograma:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Define o Range do Histograma:")) {
                    vetor = linha.split(" ");
                    //System.out.println(linha);                    
                }
            }
            txtPrecipRange.setText(vetor[5]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Define valor do limite inferior da ultima classe do histograma:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Define valor do limite inferior da ultima classe do histograma:")) {
                    vetor = linha.split(" ");
                    //System.out.println(linha);                    
                }
            }
            txtPrecipLimite.setText(vetor[10]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Define valor do minimo inferior da primeira classe do histograma:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Define valor do minimo inferior da primeira classe do histograma:")) {
                    vetor = linha.split(" ");
                    //System.out.println(linha);                    
                }
            }
            txtPrecipMinimo.setText(vetor[10]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Define qual Precipitacao deseja avaliar:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Define qual Precipitacao deseja avaliar:")) {
                    vetor = linha.split(" ");
                    //System.out.println(linha);                    
                }
            }
            txtPrecipTipo.setText(vetor[5]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Define o periodo de acumulo de precpitacao da observacao:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Define o periodo de acumulo de precpitacao da observacao:")) {
                    vetor = linha.split(" ");
                    //System.out.println(linha);                    
                }
            }
            txtPrecipAcumuloObs.setText(vetor[9]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Define o periodo de acumulo de precpitacao do experimento:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Define o periodo de acumulo de precpitacao do experimento:")) {
                    vetor = linha.split(" ");
                    //System.out.println(linha);                    
                }
            }
            txtPrecipAcumuloExp.setText(vetor[9]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }

        //Output directory:
        try {
            String linha = "";
            String[] vetor = null;
            BufferedReader in = new BufferedReader(new FileReader(arq));
            while ((linha = in.readLine()) != null) {
                if (linha.contains("Output directory:")) {
                    vetor = linha.split(" ");
                    //System.out.println(linha);                    
                }
            }
            txtEndSaida.setText(vetor[2]);

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }



    }

    //------------------------------------------------------------------------------------------------
    // ::: GRAFICOS :::
    private void leituraArqSaida() {
        String nomeArq = "";
        String linha;
        int nExp = Integer.parseInt(txtQauntExp.getText());
        int totalPrev = Integer.parseInt(txtPrevisaoTempoTotal.getText());
        int horaPrev = Integer.parseInt(txtPrevisaoTempo.getText());
        int quatHoras = (totalPrev / horaPrev) + 2;//porque + 2 para somar a zero horas e o cabeçalho
        valores = new String[nExp][quatHoras][23]; // = new String[][][];//[quantExp][quantHoras][quantVariaveis]
        //System.out.println("VALORES "+nExp+" "+quatHoras);

        //pegando data inicial e final
        Date dataI = jDateDataIni.getDate();
        String dataIni = UtilData.dateToStringSemBarra(dataI) + jComboBoxHoraIni.getSelectedItem();
        Date dataF = jDateDataFinal.getDate();
        String dataFinal = UtilData.dateToStringSemBarra(dataF) + jComboBoxHoraFinal.getSelectedItem();
        
        //diretorio saida das figuras
        Main.endDirFigSaida = Main.endSaida ;
                
        qauntExp = Integer.parseInt(txtQauntExp.getText());

        for (int w = 0; w < qauntExp; w++) {// loop qauntidade de arquivos

            if (jRadioButtonAcor.isSelected() == true) {
                int exp = w + 1;
                nomeArq = Main.endDirFigSaida + "ACOREXP0" + exp + "_" + dataIni + dataFinal + "T.scam";
            } else if (jRadioButtonRmse.isSelected() == true) {
                int exp = w + 1;
                nomeArq = Main.endDirFigSaida + "RMSEEXP0" + exp + "_" + dataIni + dataFinal + "T.scam";
            } else if (jRadioButtonVies.isSelected() == true) {
                int exp = w + 1;
                nomeArq = Main.endDirFigSaida + "VIESEXP0" + exp + "_" + dataIni + dataFinal + "T.scam";
            }
            txtArqSaida.setText(nomeArq);
            umaLinha.clear();
            //System.out.println("ARQ= " + nomeArq);
            try {
                BufferedReader in = new BufferedReader(new FileReader(nomeArq));

                while ((linha = in.readLine()) != null) {
                    umaLinha.add(linha);
                    //System.out.println(linha);

                }

                //   System.out.println(teste.get(0));

                for (int i = 0; i < umaLinha.size(); i++) {

                    if (umaLinha.get(i).contains("%")) {
                        cabecalho = umaLinha.get(i).trim().split(" ");
                        //System.out.println(cabecalho[1]);

                    } else {

                        //tirando os espacos da linha 
                        linha = umaLinha.get(i);
                        linha = linha.trim().replace("    ", " ");
                        linha = linha.trim().replace("   ", " ");
                        linha = linha.trim().replace("  ", " ");

                        val = linha.trim().split(" ");
                        for (int j = 0; j < val.length; j++) {
                            valores[w][i][j] = val[j];
                            //System.out.println(valores[w][i][j] + " w= " + w + " i= " + i + " j= " + j);
                        }


                    }

                }


            } catch (Exception e) {
                System.err.println("Erro na abertura do arquivo " + nomeArq + '\n');
                e.printStackTrace();
            }
            // System.out.println("parou aqui");
        }
        tabelaPadraoValores();
        setVariaveis();
        jComboBoxVariavelItemStateChanged(null);

    }

    private void tabelaPadraoValores() {
        //leituraArqSaida();
        DefaultTableModel modelo = ((DefaultTableModel) tabelaValores.getModel());
        modelo.setRowCount(0);
        modelo.setColumnCount(0);
        int totalPrev = Integer.parseInt(txtPrevisaoTempoTotal.getText());
        int horaPrev = Integer.parseInt(txtPrevisaoTempo.getText());
        int quatHoras = (totalPrev / horaPrev) + 2;//porque + 2 para somar a zero horas e o cabeçalho
        //System.out.println("quatHoras= "+quatHoras);

        for (int i = 1; i < quatHoras; i++) { // nomenclatura das colunas da tabela
            modelo.addColumn(valores[0][i][0]);
            //System.out.println(valores[0][i][0]);
        }
        //System.out.println("parou aqui");

    }

    private void setVariaveis() {//metodo para adicionar as variaveis no jcomboBox
        //leituraArqSaida();

        DefaultComboBoxModel modelo = ((DefaultComboBoxModel) jComboBoxVariavel.getModel());
        modelo.removeAllElements();
        for (int i = 1; i < cabecalho.length; i++) {
            modelo.addElement(cabecalho[i]);
        }

    }

    public void PlotTest() throws FileNotFoundException, IOException {
        // leituraArqSaida();
        dataset = new XYSeriesCollection();
        //Quantidade de Arquivos
        String nomeTipo = "";
        for (int w = 0; w < qauntExp; w++) { //loop quantidade de arquivos
            //System.out.println("HELLO " + nomeExp.get(w));

            XYSeries data = new XYSeries(nomeExp.get(w));
            int totalPrev = Integer.parseInt(txtPrevisaoTempoTotal.getText());
            int horaPrev = Integer.parseInt(txtPrevisaoTempo.getText());
            int quatHoras = (totalPrev / horaPrev) + 2;//porque + 2 para somar a zero horas e o cabeçalho
            int cont = 0;
            for (int j = 1; j < quatHoras; j++) {
                if (jRadioButtonAcor.isSelected() == true) {
                    //System.out.println("cont "+cont);
                    data.add(cont, Double.parseDouble(valores[w][j][jComboBoxVariavel.getSelectedIndex() + 1]) * 100); //Point 1  
                    cont = cont + horaPrev;
                } else {
                    data.add(cont, Double.parseDouble(valores[w][j][jComboBoxVariavel.getSelectedIndex() + 1])); //Point 1  
                    cont = cont + horaPrev;
                }

            }

            dataset.addSeries(data);

            if (jRadioButtonAcor.isSelected() == true) {
                nomeTipo = "ACOR_";
            } else if (jRadioButtonRmse.isSelected() == true) {
                nomeTipo = "RMSE_";
            } else if (jRadioButtonVies.isSelected() == true) {
                nomeTipo = "VIES_";
            }
        }
        showGraph(nomeTipo + cabecalho[jComboBoxVariavel.getSelectedIndex() + 1 ]+ "_" + Recorte.ReplaceEspacoPorAnderline(jComboBoxRecorte.getSelectedItem().toString()));
    }

    private void showGraph(String titulo) throws FileNotFoundException, IOException {
        final JFreeChart chart = createChart(dataset, titulo);
        final ChartPanel chartPanel = new ChartPanel(chart);
        chartPanel.setPreferredSize(new java.awt.Dimension(900, 700));
        JFrame frame = new JFrame(titulo);//Nome no topo do Frame
        frame.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        frame.setContentPane(chartPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);

    }

    private JFreeChart createChart(final XYDataset dataset, String titulo) throws FileNotFoundException, IOException {
        final JFreeChart chart = ChartFactory.createScatterPlot(
                titulo, // chart title  
                "HORAS DE INTEGRAÇÃO", // x axis label  
                "VALORES", // y axis label  
                dataset, // data  
                PlotOrientation.VERTICAL,
                true, // include legend  
                true, // tooltips  
                false // urls  
                );
        

        // Comentando essas linhas abaixo;  
        XYPlot plot = (XYPlot) chart.getPlot();
        XYLineAndShapeRenderer renderer = new XYLineAndShapeRenderer();
        renderer.setSeriesLinesVisible(0, true);
        plot.setRenderer(renderer);

        //Defenir range
        final NumberAxis rangeAxis = (NumberAxis) plot.getDomainAxis();
        rangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());

        double valMin = Double.parseDouble(valores[0][1][0]);//primeira hora
        rangeAxis.setLowerBound(valMin);

        double valPasso = Double.parseDouble(valores[0][2][0]);//passo de horas
        rangeAxis.setUpperMargin(valPasso);

        int totalPrev = Integer.parseInt(txtPrevisaoTempoTotal.getText());
        int horaPrev = Integer.parseInt(txtPrevisaoTempo.getText());
        int quatHoras = (totalPrev / horaPrev) + 1;//porque + 2 para somar a zero 
        //System.out.println("quatHoras= "+quatHoras);
        double valMax = valPasso + Double.parseDouble(valores[0][quatHoras][0]);//passo de horas
        rangeAxis.setUpperBound(valMax);
        rangeAxis.setTickUnit(new NumberTickUnit(valPasso)); //traça o intervalo entre valores   
        //System.out.println("VALORES " + valMin + " " + valPasso + " " + valMax);

        //marcando um linha reta
        double valor = 0.0;
        if (jRadioButtonAcor.isSelected() == true) {
            valor = 60.0;
        }
        Marker marcador = new ValueMarker(valor);
        chart.getXYPlot().addRangeMarker(marcador);
        marcador.setPaint(Color.BLACK);
        
        Main.endDirFigSaida = Main.endSaida;

        File pasta = new File(Main.endDirFigSaida + "images" + "/");
        if (!pasta.exists()) {
            pasta.mkdir();
        }
        
        //salvando os arquivos
        
        OutputStream arquivo = new FileOutputStream(Main.endDirFigSaida + "images" + "/" + titulo + ".png");
        ChartUtilities.writeChartAsPNG(arquivo, chart, 1000, 800);


        return chart;
    }

    //----------------------------------------------------------------------------------------------------
    //parte para salvar as figuras em disco
    public void PlotTest2(int metri) throws FileNotFoundException, IOException {
        // leituraArqSaida();

        //Quantidade de Arquivos
        String nomeTipo = "";
        for (int i = 1; i <= 22; i++) {//loop de variaveis
            dataset = new XYSeriesCollection();
            for (int w = 0; w < qauntExp; w++) { //loop quantidade de arquivos
                //System.out.println("HELLO " + nomeExp.get(w));

                XYSeries data = new XYSeries(nomeExp.get(w));
                int totalPrev = Integer.parseInt(txtPrevisaoTempoTotal.getText());
                int horaPrev = Integer.parseInt(txtPrevisaoTempo.getText());
                int quatHoras = (totalPrev / horaPrev) + 2;//porque + 2 para somar a zero horas e o cabeçalho
                int cont = 0;

                for (int j = 1; j < quatHoras; j++) {
                    if (metri == 1) {//significa que é ACOR
                        //System.out.println("W= " + w + "j= " + j + "i= " + i);
                        data.add(cont, Double.parseDouble(valores[w][j][i]) * 100); //Point 1  
                        cont = cont + horaPrev;
                    } else {
                        data.add(cont, Double.parseDouble(valores[w][j][i])); //Point 1  
                        cont = cont + horaPrev;
                    }

                }

                dataset.addSeries(data);

                if (metri == 1) {
                    nomeTipo = "ACOR_";
                } else if (metri == 2) {
                    nomeTipo = "RMSE_";
                } else if (metri == 3) {
                    nomeTipo = "VIES_";
                }
            }
            showGraph2(nomeTipo + cabecalho[i] + "_" + Recorte.ReplaceEspacoPorAnderline(jComboBoxRecorte.getSelectedItem().toString()));//-----------
        }//Fim do loop de variaveis
    }

    private void showGraph2(String titulo) throws FileNotFoundException, IOException {

        final JFreeChart chart = createChart(dataset, titulo);
        final ChartPanel chartPanel = new ChartPanel(chart);
        chartPanel.setPreferredSize(new java.awt.Dimension(900, 700));
        JFrame frame = new JFrame(titulo);//Nome no topo do Frame
        //frame.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        //frame.setContentPane(chartPanel);
        //frame.pack();
        //frame.setLocationRelativeTo(null);
        //frame.setVisible(true);

    }

    public class salvaGraficosAll implements Runnable {

        @Override
        public void run() {
            //BarraProgressoSalvaGraficos barra = new BarraProgressoSalvaGraficos();

        	
            //barra.barraGraficos.setIndeterminate(true);
            try {

                // gera todas as figuras 
                String nomeArq = "";
                String linha;
                int nExp = Integer.parseInt(txtQauntExp.getText());
                int totalPrev = Integer.parseInt(txtPrevisaoTempoTotal.getText());
                int horaPrev = Integer.parseInt(txtPrevisaoTempo.getText());
                int quatHoras = (totalPrev / horaPrev) + 2;//porque + 2 para somar a zero horas e o cabeçalho
                valores = new String[nExp][quatHoras][23]; // = new String[][][];//[quantExp][quantHoras][quantVariaveis]
                //System.out.println("VALORES "+nExp+" "+quatHoras);

                //pegando data inicial e final
                Date dataI = jDateDataIni.getDate();
                String dataIni = UtilData.dateToStringSemBarra(dataI) + jComboBoxHoraIni.getSelectedItem();
                Date dataF = jDateDataFinal.getDate();
                String dataFinal = UtilData.dateToStringSemBarra(dataF) + jComboBoxHoraFinal.getSelectedItem();
                
             
                Main.endDirFigSaida = Main.endSaida;
                
                qauntExp = Integer.parseInt(txtQauntExp.getText());

                for (int metrica = 1; metrica <= 3; metrica++) {
                    for (int w = 0; w < qauntExp; w++) {// loop qauntidade de arquivos

                        if (metrica == 1) {
                            int exp = w + 1;
                            nomeArq = Main.endDirFigSaida + "ACOREXP0" + exp + "_" + dataIni + dataFinal + "T.scam";
                        } else if (metrica == 2) {
                            int exp = w + 1;
                            nomeArq = Main.endDirFigSaida + "RMSEEXP0" + exp + "_" + dataIni + dataFinal + "T.scam";
                        } else if (metrica == 3) {
                            int exp = w + 1;
                            nomeArq = Main.endDirFigSaida + "VIESEXP0" + exp + "_" + dataIni + dataFinal + "T.scam";
                        }
                        txtArqSaida.setText(nomeArq);
                        umaLinha.clear();
                        //System.out.println("ARQ= " + nomeArq);
                        try {
                            BufferedReader in = new BufferedReader(new FileReader(nomeArq));

                            while ((linha = in.readLine()) != null) {
                                umaLinha.add(linha);
                                //System.out.println(linha);

                            }

                            //   System.out.println(teste.get(0));

                            for (int i = 0; i < umaLinha.size(); i++) {

                                if (umaLinha.get(i).contains("%")) {
                                    cabecalho = umaLinha.get(i).trim().split(" ");
                                    //System.out.println(cabecalho[1]);

                                } else {

                                    //tirando os espacos da linha 
                                    linha = umaLinha.get(i);
                                    linha = linha.trim().replace("    ", " ");
                                    linha = linha.trim().replace("   ", " ");
                                    linha = linha.trim().replace("  ", " ");

                                    val = linha.trim().split(" ");
                                    for (int j = 0; j < val.length; j++) {
                                        valores[w][i][j] = val[j];
                                        //System.out.println(valores[w][i][j] + " w= " + w + " i= " + i + " j= " + j);
                                    }


                                }

                            }


                        } catch (Exception e) {
                            System.err.println("Erro na abertura do arquivo " + nomeArq + '\n');
                            e.printStackTrace();
                        }
                        // System.out.println("parou aqui");
                    }
                    try {
                        PlotTest2(metrica);
                    } catch (FileNotFoundException ex) {
                        Logger.getLogger(ScamtecConfiguracao.class.getName()).log(Level.SEVERE, null, ex);
                    } catch (IOException ex) {
                        Logger.getLogger(ScamtecConfiguracao.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }


            } catch (Exception err) {
                err.printStackTrace();
            }
            //barra.barraGraficos.setIndeterminate(false);
            //barra.dispose();

        }
    }

    // Fim da parte para salvar em disco
    //----------------------------------------------------------------------------------------------------
    //------------------------------------------------------------------------------------------------
    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        buttonGroup1 = new javax.swing.ButtonGroup();
        jTabbedPaneGrafico = new javax.swing.JTabbedPane();
        jPanelTempoDominio = new javax.swing.JPanel();
        jPanel3 = new javax.swing.JPanel();
        txtHistoriaTempo = new javax.swing.JTextField();
        jLabel6 = new javax.swing.JLabel();
        txtPrevisaoTempoTotal = new javax.swing.JTextField();
        jLabel3 = new javax.swing.JLabel();
        txtPrevisaoTempo = new javax.swing.JTextField();
        jLabel5 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        txtAnaliseTempo = new javax.swing.JTextField();
        jLabel2 = new javax.swing.JLabel();
        jLabel1 = new javax.swing.JLabel();
        jDateDataIni = new com.toedter.calendar.JDateChooser();
        jComboBoxHoraIni = new javax.swing.JComboBox();
        jDateDataFinal = new com.toedter.calendar.JDateChooser();
        jComboBoxHoraFinal = new javax.swing.JComboBox();
        jLabel41 = new javax.swing.JLabel();
        jLabel44 = new javax.swing.JLabel();
        jLabel45 = new javax.swing.JLabel();
        jLabel46 = new javax.swing.JLabel();
        jLabel47 = new javax.swing.JLabel();
        jLabel48 = new javax.swing.JLabel();
        //btLoaderScamConf = new javax.swing.JButton();
        jPanel4 = new javax.swing.JPanel();
        jLabel7 = new javax.swing.JLabel();
        txtResolucaoY = new javax.swing.JTextField();
        jLabel12 = new javax.swing.JLabel();
        jLabel11 = new javax.swing.JLabel();
        txtResolucaoX = new javax.swing.JTextField();
        txtLatEsq = new javax.swing.JTextField();
        jLabel8 = new javax.swing.JLabel();
        txtLatDireta = new javax.swing.JTextField();
        jLabel9 = new javax.swing.JLabel();
        txtLongInf = new javax.swing.JTextField();
        jLabel10 = new javax.swing.JLabel();
        txtLongSup = new javax.swing.JTextField();
        jComboBoxRegiao = new javax.swing.JComboBox();
        jLabel16 = new javax.swing.JLabel();
        btnAdicionarRecorte = new javax.swing.JButton();
        jScrollPane4 = new javax.swing.JScrollPane();
        tabelaRecorte = new javax.swing.JTable();
        btnDeletarLinha = new javax.swing.JButton();
        jPanelPropArq = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        jComboBoxRefModelAnlise = new javax.swing.JComboBox();
        jLabel13 = new javax.swing.JLabel();
        jLabel14 = new javax.swing.JLabel();
        txtEndAnalise = new javax.swing.JTextField();
        jLabel15 = new javax.swing.JLabel();
        jPanel6 = new javax.swing.JPanel();
        jLabel17 = new javax.swing.JLabel();
        txtQauntExp = new javax.swing.JTextField();
        jScrollPane2 = new javax.swing.JScrollPane();
        tabelaEXP = new javax.swing.JTable();
        jLabel18 = new javax.swing.JLabel();
        jComboBoxRefModelExp = new javax.swing.JComboBox();
        jLabel19 = new javax.swing.JLabel();
        txtEndExp = new javax.swing.JTextField();
        jLabel20 = new javax.swing.JLabel();
        btAdicionar = new javax.swing.JButton();
        btDeletar = new javax.swing.JButton();
        txtNomeEXP = new javax.swing.JTextField();
        jLabel49 = new javax.swing.JLabel();
        jLabel50 = new javax.swing.JLabel();
        jPanelPropOpcao = new javax.swing.JPanel();
        jPanel8 = new javax.swing.JPanel();
        jRadioButtonUseClima = new javax.swing.JRadioButton();
        jLabel21 = new javax.swing.JLabel();
        jComboBoxRefModelxClima = new javax.swing.JComboBox();
        jLabel22 = new javax.swing.JLabel();
        txtEndClima = new javax.swing.JTextField();
        jLabel23 = new javax.swing.JLabel();
        jPanel9 = new javax.swing.JPanel();
        jRadioButtonUsePrecip = new javax.swing.JRadioButton();
        jLabel24 = new javax.swing.JLabel();
        jComboBoxRefModelxPrecip = new javax.swing.JComboBox();
        jLabel25 = new javax.swing.JLabel();
        txtEndPrecip = new javax.swing.JTextField();
        jLabel26 = new javax.swing.JLabel();
        jLabel27 = new javax.swing.JLabel();
        txtPrecipRange = new javax.swing.JTextField();
        jLabel28 = new javax.swing.JLabel();
        txtPrecipLimite = new javax.swing.JTextField();
        jLabel29 = new javax.swing.JLabel();
        txtPrecipMinimo = new javax.swing.JTextField();
        jLabel30 = new javax.swing.JLabel();
        txtPrecipTipo = new javax.swing.JTextField();
        jLabel31 = new javax.swing.JLabel();
        txtPrecipAcumuloObs = new javax.swing.JTextField();
        jLabel32 = new javax.swing.JLabel();
        txtPrecipAcumuloExp = new javax.swing.JTextField();
        jLabel33 = new javax.swing.JLabel();
        jLabel34 = new javax.swing.JLabel();
        jLabel35 = new javax.swing.JLabel();
        jLabel36 = new javax.swing.JLabel();
        jLabel37 = new javax.swing.JLabel();
        jLabel38 = new javax.swing.JLabel();
        jPanel10 = new javax.swing.JPanel();
        jLabel39 = new javax.swing.JLabel();
        txtEndSaida = new javax.swing.JTextField();
        jLabel40 = new javax.swing.JLabel();
        jPanel11 = new javax.swing.JPanel();
        jLabel42 = new javax.swing.JLabel();
        txtEndExecutavel = new javax.swing.JTextField();
        jLabel43 = new javax.swing.JLabel();
        jPanelRun = new javax.swing.JPanel();
        jPanel12 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        txtResult = new javax.swing.JTextArea();
        barraProcesso = new javax.swing.JProgressBar();
        btRun = new javax.swing.JButton();
        jPanelGrafico = new javax.swing.JPanel();
        jScrollPane3 = new javax.swing.JScrollPane();
        tabelaValores = new javax.swing.JTable();
        btGrafico = new javax.swing.JButton();
        jRadioButtonAcor = new javax.swing.JRadioButton();
        jComboBoxVariavel = new javax.swing.JComboBox();
        jRadioButtonVies = new javax.swing.JRadioButton();
        jRadioButtonRmse = new javax.swing.JRadioButton();
        txtArqSaida = new javax.swing.JTextField();
        btLoadArqSaida = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();
        geraGraficoGeralEmHtml = new javax.swing.JButton();
        jComboBoxRecorte = new javax.swing.JComboBox();
        jPanel1 = new javax.swing.JPanel();
        txtVIES2 = new javax.swing.JTextField();
        txtRMS2 = new javax.swing.JTextField();
        txtACOR2 = new javax.swing.JTextField();
        txtACOR1 = new javax.swing.JTextField();
        txtRMS1 = new javax.swing.JTextField();
        txtVIES1 = new javax.swing.JTextField();
        jLabel52 = new javax.swing.JLabel();
        jLabel56 = new javax.swing.JLabel();
        jLabel54 = new javax.swing.JLabel();
        jLabel51 = new javax.swing.JLabel();
        jLabel53 = new javax.swing.JLabel();
        jLabel55 = new javax.swing.JLabel();

        setClosable(true);
        setTitle("SCAMTEC CONFIGURAÇÕES");

        jTabbedPaneGrafico.setName("a"); // NOI18N
        jTabbedPaneGrafico.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jTabbedPaneGraficoMouseClicked(evt);
            }
        });

        txtHistoriaTempo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                txtHistoriaTempoActionPerformed(evt);
            }
        });

        jLabel6.setText("HISTORIA TEMPO:");

        jLabel3.setText("ANALISE PASSO TEMPO:");

        jLabel5.setText("TEMPO TOTAL DE PREVISÃO:");

        jLabel4.setText("PREVISÃO PASSO TEMPO:");

        jLabel2.setText("DATA FINAL:");

        jLabel1.setText("DATA INICIAL:");

        jDateDataIni.setDateFormatString("yyyyMMdd");

        jComboBoxHoraIni.setEditable(true);
        jComboBoxHoraIni.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24" }));

        jDateDataFinal.setDateFormatString("yyyyMMdd");

        jComboBoxHoraFinal.setEditable(true);
        jComboBoxHoraFinal.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24" }));
        jComboBoxHoraFinal.setSelectedIndex(18);

        jLabel41.setText("hh");

        jLabel44.setText("hh");

        jLabel45.setText("hh");

        jLabel46.setText("hh");

        jLabel47.setText("hh");

        jLabel48.setText("hh");

       /* Comentando o botao loader (carregamento de um conf pronto)
        btLoaderScamConf.setIcon(new javax.swing.ImageIcon(getClass().getResource("/br/scamtec/imagens/text.png"))); // NOI18N
        btLoaderScamConf.setText("LOADER");
        btLoaderScamConf.setHorizontalTextPosition(javax.swing.SwingConstants.RIGHT);
        btLoaderScamConf.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btLoaderScamConfActionPerformed(evt);
            }
        });
         */

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel3, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel4, javax.swing.GroupLayout.Alignment.TRAILING))
                .addGap(3, 3, 3)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(txtAnaliseTempo, javax.swing.GroupLayout.PREFERRED_SIZE, 39, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(txtPrevisaoTempo, javax.swing.GroupLayout.PREFERRED_SIZE, 39, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(4, 4, 4)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(jLabel48, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel45, javax.swing.GroupLayout.Alignment.TRAILING))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(jLabel5)
                        .addGap(4, 4, 4)
                        .addComponent(txtPrevisaoTempoTotal, javax.swing.GroupLayout.PREFERRED_SIZE, 39, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(4, 4, 4)
                        .addComponent(jLabel46))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(jLabel6)
                        .addGap(4, 4, 4)
                        .addComponent(txtHistoriaTempo, javax.swing.GroupLayout.PREFERRED_SIZE, 39, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(4, 4, 4)
                        .addComponent(jLabel47)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(jLabel2)
                        .addGap(4, 4, 4)
                        .addComponent(jDateDataFinal, javax.swing.GroupLayout.PREFERRED_SIZE, 130, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jComboBoxHoraFinal, javax.swing.GroupLayout.PREFERRED_SIZE, 52, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(4, 4, 4)
                        .addComponent(jLabel44))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addGap(4, 4, 4)
                        .addComponent(jDateDataIni, javax.swing.GroupLayout.PREFERRED_SIZE, 130, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jComboBoxHoraIni, javax.swing.GroupLayout.PREFERRED_SIZE, 52, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(4, 4, 4)
                        .addComponent(jLabel41)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                //.addComponent(btLoaderScamConf)
                .addContainerGap(384, Short.MAX_VALUE))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGap(1, 1, 1)
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                            .addComponent(jLabel5)
                            .addComponent(txtPrevisaoTempoTotal, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel46)
                            .addComponent(jLabel1)
                            .addComponent(jDateDataIni, javax.swing.GroupLayout.PREFERRED_SIZE, 24, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jComboBoxHoraIni, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel41))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                            .addComponent(jLabel6)
                            .addComponent(txtHistoriaTempo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel47)
                            .addComponent(jLabel2)
                            .addComponent(jDateDataFinal, javax.swing.GroupLayout.PREFERRED_SIZE, 24, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jComboBoxHoraFinal, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel44)))
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addContainerGap()
                        //.addComponent(btLoaderScamConf))
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGap(3, 3, 3)
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                            .addComponent(jLabel3)
                            .addComponent(txtAnaliseTempo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel45))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                            .addComponent(jLabel4)
                            .addComponent(txtPrevisaoTempo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel48))))
                //.addContainerGap(16, Short.MAX_VALUE))
        )));

        jLabel7.setText("LATITUDE ESQUERDA:");

        jLabel12.setText("RESOLUÇÃO DY:");

        jLabel11.setText("RESOLUÇÃO DX:");

        jLabel8.setText("LATITUDE DIREITA:");

        jLabel9.setText("LONGITUDE INFERIOR:");

        jLabel10.setText("LONGITUDE SUPERIOR:");

        jComboBoxRegiao.setEditable(true);
        jComboBoxRegiao.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "AMERICA DO SUL", "HEMISFERIO SUL", "BRASIL", "SÃO PAULO", "NOVO" }));
        jComboBoxRegiao.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxRegiaoActionPerformed(evt);
            }
        });

        jLabel16.setText("REGIÕES PRÉ CONFIGURADO");

        btnAdicionarRecorte.setIcon(new javax.swing.ImageIcon(getClass().getResource("/br/scamtec/imagens/check.png"))); // NOI18N
        btnAdicionarRecorte.setText("Adicionar Recorte");
        btnAdicionarRecorte.setName(""); // NOI18N
        btnAdicionarRecorte.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnAdicionarRecorteActionPerformed(evt);
            }
        });

        tabelaRecorte.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {

            },
            new String [] {
                "Nº", "Nome da Área", "Latitude Esquerda", "latitude Direita", "Logitude Inferior", "Longitude Superior", "Resolução DX", "Resolução DY"
            }
        ));
        jScrollPane4.setViewportView(tabelaRecorte);

        btnDeletarLinha.setIcon(new javax.swing.ImageIcon(getClass().getResource("/br/scamtec/imagens/delete.png"))); // NOI18N
        btnDeletarLinha.setText("Deletar Recorte");
        btnDeletarLinha.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnDeletarLinhaActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane4, javax.swing.GroupLayout.DEFAULT_SIZE, 1294, Short.MAX_VALUE)
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                .addComponent(jLabel8)
                                .addComponent(jLabel7))
                            .addComponent(btnAdicionarRecorte))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(btnDeletarLinha)
                            .addGroup(jPanel4Layout.createSequentialGroup()
                                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(txtLatEsq, javax.swing.GroupLayout.PREFERRED_SIZE, 83, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addComponent(txtLatDireta, javax.swing.GroupLayout.PREFERRED_SIZE, 83, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jLabel9, javax.swing.GroupLayout.Alignment.TRAILING)
                                    .addComponent(jLabel10, javax.swing.GroupLayout.Alignment.TRAILING))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(txtLongInf, javax.swing.GroupLayout.PREFERRED_SIZE, 101, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addComponent(txtLongSup, javax.swing.GroupLayout.PREFERRED_SIZE, 101, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jLabel11, javax.swing.GroupLayout.Alignment.TRAILING)
                                    .addComponent(jLabel12, javax.swing.GroupLayout.Alignment.TRAILING))
                                .addGap(1, 1, 1)
                                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(txtResolucaoX, javax.swing.GroupLayout.PREFERRED_SIZE, 67, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addComponent(txtResolucaoY, javax.swing.GroupLayout.PREFERRED_SIZE, 67, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jLabel16)
                                    .addComponent(jComboBoxRegiao, javax.swing.GroupLayout.PREFERRED_SIZE, 158, javax.swing.GroupLayout.PREFERRED_SIZE))))))
                .addContainerGap())
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel7)
                    .addComponent(txtLatEsq, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel9)
                    .addComponent(txtLongInf, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel11)
                    .addComponent(txtResolucaoX, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel16))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(txtLatDireta, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel10)
                    .addComponent(txtLongSup, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel12)
                    .addComponent(txtResolucaoY, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jComboBoxRegiao, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel8))
                .addGap(18, 18, 18)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnAdicionarRecorte)
                    .addComponent(btnDeletarLinha))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane4, javax.swing.GroupLayout.DEFAULT_SIZE, 475, Short.MAX_VALUE)
                .addContainerGap())
        );

        javax.swing.GroupLayout jPanelTempoDominioLayout = new javax.swing.GroupLayout(jPanelTempoDominio);
        jPanelTempoDominio.setLayout(jPanelTempoDominioLayout);
        jPanelTempoDominioLayout.setHorizontalGroup(
            jPanelTempoDominioLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelTempoDominioLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelTempoDominioLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanelTempoDominioLayout.setVerticalGroup(
            jPanelTempoDominioLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelTempoDominioLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTabbedPaneGrafico.addTab("PROPRIEDADES DO TEMPO E DOMINIO", jPanelTempoDominio);

        jComboBoxRefModelAnlise.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "AGCM/CPTEC", "IWV-PSAS T126", "Precipitation", " " }));

        jLabel13.setText("SELECIONE O MODELO DE ANALISE (REFERENCIA)");

        jLabel14.setText("DIRETÓRIO:");

        jLabel15.setText("Exemplo: /diretório/%y4%m2%d2%h2/GPOSNMC%y4%m2%d2%h2%y4%m2%d2%h2P.icn.TQ0299L064.grb");

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(jLabel13, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jComboBoxRefModelAnlise, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel14)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel15, javax.swing.GroupLayout.DEFAULT_SIZE, 942, Short.MAX_VALUE)
                    .addComponent(txtEndAnalise, javax.swing.GroupLayout.DEFAULT_SIZE, 942, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel13)
                    .addComponent(jLabel15))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jComboBoxRefModelAnlise, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel14)
                    .addComponent(txtEndAnalise, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(21, Short.MAX_VALUE))
        );

        jLabel17.setText("QUANTIDADE DE EXP:");

        tabelaEXP.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        tabelaEXP.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                tabelaEXPMouseClicked(evt);
            }
        });
        jScrollPane2.setViewportView(tabelaEXP);

        jLabel18.setText("SELECIONE O MODELO DO EXPERIMENTO");

        jComboBoxRefModelExp.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "AGCM/CPTEC", "IWV-PSAS T126", "Precipitation", " " }));

        jLabel19.setText("DIRETÓRIO:");

        jLabel20.setText("Exemplo: /diretório/TQ0299L064/%y4%m2%d2%h2/GPOSNMC%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2P.fct.TQ0299L064.grb");

        btAdicionar.setIcon(new javax.swing.ImageIcon(getClass().getResource("/br/scamtec/imagens/check.png"))); // NOI18N
        btAdicionar.setText("ADICIONAR");
        btAdicionar.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btAdicionarActionPerformed(evt);
            }
        });

        btDeletar.setIcon(new javax.swing.ImageIcon(getClass().getResource("/br/scamtec/imagens/delete.png"))); // NOI18N
        btDeletar.setText("DELETAR");
        btDeletar.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btDeletarActionPerformed(evt);
            }
        });

        jLabel49.setText("NOME:");

        jLabel50.setText("EX: C/GPS");

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 1294, Short.MAX_VALUE)
                    .addComponent(jLabel18, javax.swing.GroupLayout.DEFAULT_SIZE, 1294, Short.MAX_VALUE)
                    .addGroup(jPanel6Layout.createSequentialGroup()
                        .addComponent(jLabel17)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(txtQauntExp, javax.swing.GroupLayout.PREFERRED_SIZE, 51, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(btAdicionar)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btDeletar)
                        .addGap(0, 851, Short.MAX_VALUE))
                    .addGroup(jPanel6Layout.createSequentialGroup()
                        .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addGroup(jPanel6Layout.createSequentialGroup()
                                .addComponent(jComboBoxRefModelExp, javax.swing.GroupLayout.PREFERRED_SIZE, 276, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabel19))
                            .addComponent(jLabel49))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel20, javax.swing.GroupLayout.DEFAULT_SIZE, 911, Short.MAX_VALUE)
                            .addComponent(txtEndExp, javax.swing.GroupLayout.DEFAULT_SIZE, 911, Short.MAX_VALUE)
                            .addGroup(jPanel6Layout.createSequentialGroup()
                                .addComponent(txtNomeEXP, javax.swing.GroupLayout.PREFERRED_SIZE, 409, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabel50)
                                .addGap(0, 423, Short.MAX_VALUE)))))
                .addContainerGap())
        );
        jPanel6Layout.setVerticalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel17)
                    .addComponent(txtQauntExp, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btAdicionar)
                    .addComponent(btDeletar))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel18)
                    .addComponent(jLabel20))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jComboBoxRefModelExp, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel19)
                    .addComponent(txtEndExp, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(txtNomeEXP, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel49)
                    .addComponent(jLabel50))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 471, Short.MAX_VALUE)
                .addContainerGap())
        );

        javax.swing.GroupLayout jPanelPropArqLayout = new javax.swing.GroupLayout(jPanelPropArq);
        jPanelPropArq.setLayout(jPanelPropArqLayout);
        jPanelPropArqLayout.setHorizontalGroup(
            jPanelPropArqLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelPropArqLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelPropArqLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel5, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanelPropArqLayout.setVerticalGroup(
            jPanelPropArqLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelPropArqLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTabbedPaneGrafico.addTab("PROPRIEDADES DOS ARQUIVOS", jPanelPropArq);

        jRadioButtonUseClima.setText("USE CLIMATOLOGIA");
        jRadioButtonUseClima.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jRadioButtonUseClimaMouseClicked(evt);
            }
        });

        jLabel21.setText("MODELO DE CLIMATOLOGIA");

        jComboBoxRefModelxClima.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "50yr Climatology" }));
        jComboBoxRefModelxClima.setFocusable(false);

        jLabel22.setText("DIRETÓRIO:");

        jLabel23.setText("/diretório/climatologiaJGM/climatologia50yr.%mc.bin");

        javax.swing.GroupLayout jPanel8Layout = new javax.swing.GroupLayout(jPanel8);
        jPanel8.setLayout(jPanel8Layout);
        jPanel8Layout.setHorizontalGroup(
            jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel8Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(jRadioButtonUseClima)
                    .addComponent(jLabel21, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jComboBoxRefModelxClima, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel22)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel8Layout.createSequentialGroup()
                        .addComponent(jLabel23)
                        .addGap(0, 613, Short.MAX_VALUE))
                    .addComponent(txtEndClima, javax.swing.GroupLayout.DEFAULT_SIZE, 990, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel8Layout.setVerticalGroup(
            jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel8Layout.createSequentialGroup()
                .addComponent(jRadioButtonUseClima)
                .addGroup(jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel8Layout.createSequentialGroup()
                        .addGap(10, 10, 10)
                        .addComponent(jLabel23)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel22)
                            .addComponent(txtEndClima, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                    .addGroup(jPanel8Layout.createSequentialGroup()
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel21)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jComboBoxRefModelxClima, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jRadioButtonUsePrecip.setText("USE PRECIPITAÇÃO");
        jRadioButtonUsePrecip.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jRadioButtonUsePrecipMouseClicked(evt);
            }
        });

         jLabel24.setText("ARQUIVO PRECIPITAÇÃO");

        jComboBoxRefModelxPrecip.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Precipitation" }));
        jComboBoxRefModelxPrecip.setFocusable(false);

        jLabel25.setText("DIRETÓRIO:");

        jLabel26.setText("/endereco/preciptacao/%y4/3B42RT_SA.%y4%m2%d2%h200.bin");

        jLabel27.setText("DEFINE O RANGE:");

        jLabel28.setText("DEFINE O LIMITE:");

        jLabel29.setText("DEFINE O MINIMO:");

        jLabel30.setText("DEFINE O TIPO:");

        jLabel31.setText("DEFINE O ACUMULO DE PRECIPITAÇÃO OBS:");

        jLabel32.setText("DEFINE O ACUMULO DE PRECIPITAÇÃO EXP:");

        jLabel33.setText("EX:2");

        jLabel34.setText("EX:70");

        jLabel35.setText("EX:0");

        jLabel36.setText("16 P/ TOTAL OU 17 P/ CONVECTIVE");

        jLabel37.setText("EXP:3");

        jLabel38.setText("EXP:24");

        javax.swing.GroupLayout jPanel9Layout = new javax.swing.GroupLayout(jPanel9);
        jPanel9.setLayout(jPanel9Layout);
        jPanel9Layout.setHorizontalGroup(
            jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel9Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel28)
                    .addComponent(jLabel29)
                    .addComponent(jLabel27))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel9Layout.createSequentialGroup()
                        .addComponent(txtPrecipRange, javax.swing.GroupLayout.PREFERRED_SIZE, 51, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel33))
                    .addGroup(jPanel9Layout.createSequentialGroup()
                        .addComponent(txtPrecipLimite, javax.swing.GroupLayout.PREFERRED_SIZE, 51, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel34))
                    .addGroup(jPanel9Layout.createSequentialGroup()
                        .addComponent(txtPrecipMinimo, javax.swing.GroupLayout.PREFERRED_SIZE, 51, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel35)))
                .addGap(18, 18, 18)
                .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel31)
                    .addComponent(jLabel32))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel9Layout.createSequentialGroup()
                        .addComponent(txtPrecipAcumuloObs, javax.swing.GroupLayout.PREFERRED_SIZE, 51, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel37))
                    .addGroup(jPanel9Layout.createSequentialGroup()
                        .addComponent(txtPrecipAcumuloExp, javax.swing.GroupLayout.PREFERRED_SIZE, 51, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel38)))
                .addGap(0, 623, Short.MAX_VALUE))
            .addGroup(jPanel9Layout.createSequentialGroup()
                .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel9Layout.createSequentialGroup()
                        .addGap(12, 12, 12)
                        .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel9Layout.createSequentialGroup()
                                .addComponent(jComboBoxRefModelxPrecip, javax.swing.GroupLayout.PREFERRED_SIZE, 189, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabel25))
                            .addComponent(jLabel24))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel9Layout.createSequentialGroup()
                                .addComponent(jLabel26, javax.swing.GroupLayout.PREFERRED_SIZE, 792, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addGap(0, 206, Short.MAX_VALUE))
                            .addComponent(txtEndPrecip, javax.swing.GroupLayout.DEFAULT_SIZE, 998, Short.MAX_VALUE)))
                    .addGroup(jPanel9Layout.createSequentialGroup()
                        .addGap(12, 12, 12)
                        .addComponent(jRadioButtonUsePrecip))
                    .addGroup(jPanel9Layout.createSequentialGroup()
                        .addGap(268, 268, 268)
                        .addComponent(jLabel30)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(txtPrecipTipo, javax.swing.GroupLayout.PREFERRED_SIZE, 51, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel36)))
                .addContainerGap())
        );
        jPanel9Layout.setVerticalGroup(
            jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel9Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jRadioButtonUsePrecip)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel24)
                    .addComponent(jLabel26))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jComboBoxRefModelxPrecip, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel25)
                    .addComponent(txtEndPrecip, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel27)
                    .addComponent(txtPrecipRange, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel33)
                    .addComponent(jLabel30)
                    .addComponent(txtPrecipTipo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel36))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel28)
                    .addComponent(txtPrecipLimite, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel34)
                    .addComponent(jLabel31)
                    .addComponent(txtPrecipAcumuloObs, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel37))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel32)
                        .addComponent(txtPrecipAcumuloExp, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(jLabel38))
                    .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel29)
                        .addComponent(txtPrecipMinimo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(jLabel35)))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jLabel39.setText("RESULTADOS:");

        txtEndSaida.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusLost(java.awt.event.FocusEvent evt) {
                txtEndSaidaFocusLost(evt);
            }
        });

        jLabel40.setText("Exemplo: /scratchout/grupos/assim_dados/home/lucas.amarante/resultados_scamtec/");

        javax.swing.GroupLayout jPanel10Layout = new javax.swing.GroupLayout(jPanel10);
        jPanel10.setLayout(jPanel10Layout);
        jPanel10Layout.setHorizontalGroup(
            jPanel10Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel10Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel39)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel10Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel10Layout.createSequentialGroup()
                        .addComponent(jLabel40)
                        .addGap(0, 578, Short.MAX_VALUE))
                    .addComponent(txtEndSaida, javax.swing.GroupLayout.DEFAULT_SIZE, 1199, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel10Layout.setVerticalGroup(
            jPanel10Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel10Layout.createSequentialGroup()
                .addComponent(jLabel40)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel10Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel39)
                    .addComponent(txtEndSaida, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jLabel42.setText("EXECUTÁVEL (local do arquivo scamtec.x):");

        jLabel43.setText("Exemplo: /scratchin/grupos/assim_dados/home/lucas.amarante/projeto_scamtec/SCAMTEC/core/");

        javax.swing.GroupLayout jPanel11Layout = new javax.swing.GroupLayout(jPanel11);
        jPanel11.setLayout(jPanel11Layout);
        jPanel11Layout.setHorizontalGroup(
            jPanel11Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel11Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel42)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel11Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel11Layout.createSequentialGroup()
                        .addComponent(jLabel43)
                        .addGap(0, 632, Short.MAX_VALUE))
                    .addComponent(txtEndExecutavel, javax.swing.GroupLayout.DEFAULT_SIZE, 1199, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel11Layout.setVerticalGroup(
            jPanel11Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel11Layout.createSequentialGroup()
                .addComponent(jLabel43)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel11Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel42)
                    .addComponent(txtEndExecutavel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanelPropOpcaoLayout = new javax.swing.GroupLayout(jPanelPropOpcao);
        jPanelPropOpcao.setLayout(jPanelPropOpcaoLayout);
        jPanelPropOpcaoLayout.setHorizontalGroup(
            jPanelPropOpcaoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelPropOpcaoLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelPropOpcaoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel8, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel9, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel10, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel11, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanelPropOpcaoLayout.setVerticalGroup(
            jPanelPropOpcaoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelPropOpcaoLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel8, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(1, 1, 1)
                .addComponent(jPanel9, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel10, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel11, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(253, Short.MAX_VALUE))
        );

        jTabbedPaneGrafico.addTab("PROPRIEDADES DE OPÇÃO", jPanelPropOpcao);

        txtResult.setColumns(20);
        txtResult.setRows(5);
        txtResult.setText("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
        jScrollPane1.setViewportView(txtResult);

        javax.swing.GroupLayout jPanel12Layout = new javax.swing.GroupLayout(jPanel12);
        jPanel12.setLayout(jPanel12Layout);
        jPanel12Layout.setHorizontalGroup(
            jPanel12Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel12Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 1294, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanel12Layout.setVerticalGroup(
            jPanel12Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel12Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 630, Short.MAX_VALUE)
                .addContainerGap())
        );

        barraProcesso.setStringPainted(true);

        btRun.setIcon(new javax.swing.ImageIcon(getClass().getResource("/br/scamtec/imagens/gear.png"))); // NOI18N
        btRun.setText("RUN");
        btRun.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btRunActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanelRunLayout = new javax.swing.GroupLayout(jPanelRun);
        jPanelRun.setLayout(jPanelRunLayout);
        jPanelRunLayout.setHorizontalGroup(
            jPanelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelRunLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel12, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelRunLayout.createSequentialGroup()
                        .addComponent(barraProcesso, javax.swing.GroupLayout.PREFERRED_SIZE, 396, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 855, Short.MAX_VALUE)
                        .addComponent(btRun)))
                .addContainerGap())
        );
        jPanelRunLayout.setVerticalGroup(
            jPanelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelRunLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel12, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btRun)
                    .addComponent(barraProcesso, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        jTabbedPaneGrafico.addTab("RUN", jPanelRun);

        tabelaValores.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jScrollPane3.setViewportView(tabelaValores);

        btGrafico.setIcon(new javax.swing.ImageIcon(getClass().getResource("/br/scamtec/imagens/line-chart.png"))); // NOI18N
        btGrafico.setText("Grafico");
        btGrafico.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btGraficoActionPerformed(evt);
            }
        });

        buttonGroup1.add(jRadioButtonAcor);
        jRadioButtonAcor.setText("ACOR");
        jRadioButtonAcor.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jRadioButtonAcorMouseClicked(evt);
            }
        });

        jComboBoxVariavel.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        jComboBoxVariavel.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                jComboBoxVariavelItemStateChanged(evt);
            }
        });

        buttonGroup1.add(jRadioButtonVies);
        jRadioButtonVies.setText("VIES");
        jRadioButtonVies.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jRadioButtonViesMouseClicked(evt);
            }
        });

        buttonGroup1.add(jRadioButtonRmse);
        jRadioButtonRmse.setText("RMSE");
        jRadioButtonRmse.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jRadioButtonRmseMouseClicked(evt);
            }
        });

        btLoadArqSaida.setIcon(new javax.swing.ImageIcon(getClass().getResource("/br/scamtec/imagens/text.png"))); // NOI18N
        btLoadArqSaida.setText("LOAD");
        btLoadArqSaida.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btLoadArqSaidaActionPerformed(evt);
            }
        });

        jButton1.setIcon(new javax.swing.ImageIcon(getClass().getResource("/br/scamtec/imagens/disk_blue.png"))); // NOI18N
        jButton1.setText("Gerar todas Figuras");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        geraGraficoGeralEmHtml.setText("Gerar Gráfico de Comparação");
        geraGraficoGeralEmHtml.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                geraGraficoGeralEmHtmlActionPerformed(evt);
            }
        });

        jComboBoxRecorte.setEditable(true);
        jComboBoxRecorte.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "AMERICA DO SUL", "HEMISFERIO SUL", "BRASIL", "SÃO PAULO", "NOVO" }));
        jComboBoxRecorte.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxRecorteActionPerformed(evt);
            }
        });

        jPanel1.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(-16777216,true)));

        txtVIES2.setToolTipText("Escreva o diretório do experimento VIES2 com .scam no final");
        txtVIES2.setAutoscrolls(false);
        txtVIES2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                txtVIES2ActionPerformed(evt);
            }
        });

        txtRMS2.setToolTipText("Escreva o diretório do experimento RMS2 com .scam no final");
        txtRMS2.setAutoscrolls(false);

        txtACOR2.setToolTipText("Escreva o diretório do experimento ACOR2 com .scam no final");
        txtACOR2.setAutoscrolls(false);
        txtACOR2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                txtACOR2ActionPerformed(evt);
            }
        });

        txtACOR1.setToolTipText("Escreva o diretório do experimento ACOR1 com .scam no final");

        txtRMS1.setToolTipText("Escreva o diretório do experimento RMS1 com .scam no final");
        txtRMS1.setAutoscrolls(false);
        txtRMS1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                txtRMS1ActionPerformed(evt);
            }
        });

        txtVIES1.setToolTipText("Escreva o diretório do experimento VIES1 com .scam no final");
        txtVIES1.setAutoscrolls(false);

        jLabel52.setText("ACOR");

        jLabel56.setText("VIES");

        jLabel54.setText("RMS");

        jLabel51.setText("Diretórios");

        jLabel53.setText("Exp01");

        jLabel55.setText("Exp02");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addGap(22, 22, 22)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel52)
                            .addComponent(jLabel54)
                            .addComponent(jLabel56))
                        .addGap(18, 18, 18)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                            .addComponent(txtVIES1)
                            .addComponent(txtRMS1)
                            .addComponent(txtACOR1, javax.swing.GroupLayout.PREFERRED_SIZE, 587, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addGap(18, 18, 18)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                            .addComponent(txtVIES2)
                            .addComponent(txtRMS2)
                            .addComponent(txtACOR2, javax.swing.GroupLayout.DEFAULT_SIZE, 566, Short.MAX_VALUE))
                        .addContainerGap(78, Short.MAX_VALUE))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel51)
                        .addGap(240, 240, 240)
                        .addComponent(jLabel53)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 616, Short.MAX_VALUE)
                        .addComponent(jLabel55)
                        .addGap(297, 297, 297))))
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                .addContainerGap(15, Short.MAX_VALUE)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(txtACOR2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(18, 18, 18)
                        .addComponent(txtRMS2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(18, 18, 18)
                        .addComponent(txtVIES2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel51)
                            .addComponent(jLabel55)
                            .addComponent(jLabel53))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel52)
                            .addComponent(txtACOR1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addGap(18, 18, 18)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel54)
                            .addComponent(txtRMS1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addGap(18, 18, 18)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel56)
                            .addComponent(txtVIES1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))))
                .addContainerGap())
        );

        javax.swing.GroupLayout jPanelGraficoLayout = new javax.swing.GroupLayout(jPanelGrafico);
        jPanelGrafico.setLayout(jPanelGraficoLayout);
        jPanelGraficoLayout.setHorizontalGroup(
            jPanelGraficoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelGraficoLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelGraficoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanelGraficoLayout.createSequentialGroup()
                        .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addContainerGap())
                    .addGroup(jPanelGraficoLayout.createSequentialGroup()
                        .addGroup(jPanelGraficoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jScrollPane3, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.PREFERRED_SIZE, 1330, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addGroup(jPanelGraficoLayout.createSequentialGroup()
                                .addComponent(jRadioButtonVies)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jRadioButtonRmse)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jRadioButtonAcor)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jComboBoxVariavel, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jComboBoxRecorte, javax.swing.GroupLayout.PREFERRED_SIZE, 206, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btLoadArqSaida)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(txtArqSaida, javax.swing.GroupLayout.PREFERRED_SIZE, 706, javax.swing.GroupLayout.PREFERRED_SIZE)))
                        .addContainerGap())
                    .addGroup(jPanelGraficoLayout.createSequentialGroup()
                        .addComponent(geraGraficoGeralEmHtml)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 819, Short.MAX_VALUE)
                        .addComponent(jButton1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btGrafico)
                        .addGap(41, 41, 41))))
        );
        jPanelGraficoLayout.setVerticalGroup(
            jPanelGraficoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelGraficoLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelGraficoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jRadioButtonVies)
                    .addComponent(jRadioButtonRmse)
                    .addComponent(jRadioButtonAcor)
                    .addComponent(jComboBoxVariavel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(txtArqSaida, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btLoadArqSaida)
                    .addComponent(jComboBoxRecorte, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane3, javax.swing.GroupLayout.PREFERRED_SIZE, 132, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(27, 27, 27)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 49, Short.MAX_VALUE)
                .addGroup(jPanelGraficoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(geraGraficoGeralEmHtml)
                    .addComponent(btGrafico)
                    .addComponent(jButton1))
                .addGap(245, 245, 245))
        );

        jTabbedPaneGrafico.addTab("GRÁFICOS", jPanelGrafico);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jTabbedPaneGrafico, javax.swing.GroupLayout.PREFERRED_SIZE, 1350, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jTabbedPaneGrafico, javax.swing.GroupLayout.DEFAULT_SIZE, 756, Short.MAX_VALUE)
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

private void jTabbedPaneGraficoMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jTabbedPaneGraficoMouseClicked
    // System.out.println("EVENTO");
    //leituraArqSaida();
    //setVariaveis();
    //tabelaPadraoValores();
}//GEN-LAST:event_jTabbedPaneGraficoMouseClicked

private void btRunActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btRunActionPerformed
    indiceDaListaDeRecortes = listaRecorte.size() - 1;
    ExecutarScamtec(indiceDaListaDeRecortes);
}//GEN-LAST:event_btRunActionPerformed

private void txtEndSaidaFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_txtEndSaidaFocusLost
    Main.endSaida = txtEndSaida.getText();
}//GEN-LAST:event_txtEndSaidaFocusLost

private void jRadioButtonUsePrecipMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jRadioButtonUsePrecipMouseClicked
    if (jRadioButtonUsePrecip.isSelected() == true) {
        txtEndPrecip.setEditable(true);
        usePrecip = 1;

    } else {
        txtEndClima.setEditable(false);
        usePrecip = 0;
    }
}//GEN-LAST:event_jRadioButtonUsePrecipMouseClicked

private void jRadioButtonUseClimaMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jRadioButtonUseClimaMouseClicked
    if (jRadioButtonUseClima.isSelected() == true) {
        useClima = 1;

    } else {
        useClima = 0;
    }
}//GEN-LAST:event_jRadioButtonUseClimaMouseClicked


private void btDeletarActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btDeletarActionPerformed
    DefaultTableModel modelo = (DefaultTableModel) tabelaEXP.getModel();
    int linha = tabelaEXP.getSelectedRow();
        
    
	if(linha == -1) {
      JOptionPane.showMessageDialog(rootPane, "NENHUM EXPERIMENTO SELECIONADO PARA DELETAR");
    } 
	else{
    
      //String id = modelo.getValueAt(linha, 0).toString();
      modelo.removeRow(linha);
      //id_linhaTabelaExp = 0;
      txtEndExp.setText("");
      txtNomeEXP.setText("");
	}
    
    
}//GEN-LAST:event_btDeletarActionPerformed

private void btAdicionarActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btAdicionarActionPerformed
    DefaultTableModel modelo = ((DefaultTableModel) tabelaEXP.getModel());

    int contLinhaTabela = modelo.getRowCount() + 1;
    //System.out.println("Contalinha "+contLinhaTabela+"id linha: "+id_linhaTabelaExp);
    
    boolean ehNumero = true;
     

   if (txtQauntExp.getText().equals("")) {
            JOptionPane.showMessageDialog(rootPane, "INFORME A QUANTIDADE DE EXPERIMENTO");
            txtQauntExp.requestFocus();
    } else if (txtEndExp.getText().equals("")) {
            JOptionPane.showMessageDialog(rootPane, "INFORME O ENDEREÇO DO EXPERIMENTO");
            txtEndExp.requestFocus();
    } else if (txtNomeEXP.getText().equals("")) {
            JOptionPane.showMessageDialog(rootPane, "INFORME O NOME DO EXPERIMENTO");
            txtNomeEXP.requestFocus();
    } else if (id_linhaTabelaExp == -1) {
        	
     //verificando se é numeros     	
      try{
            Integer.parseInt(txtQauntExp.getText());
          	
            contExp = modelo.getRowCount();
            String[] dados = new String[13];
            int idModel = jComboBoxRefModelExp.getSelectedIndex() + 1;

            dados[0] = "" + idModel;
            dados[1] = "" + jComboBoxRefModelExp.getSelectedItem();
            contExp = contExp + 1;
            dados[2] = "EXP0" + contExp;
            dados[3] = txtEndExp.getText();
            dados[4] = txtNomeEXP.getText();
            nomeExp.add(txtNomeEXP.getText());

            if ((contLinhaTabela > Integer.parseInt(txtQauntExp.getText())) && (id_linhaTabelaExp == -1)) {
                  JOptionPane.showMessageDialog(rootPane, "JA INSERIDO TODOS OS EXPERIMENTOS");
                  txtEndExp.setText("");
                  txtNomeEXP.setText("");
                    
                }
            else{
                  modelo.addRow(dados);
                  id_linhaTabelaExp = -1;
                  txtEndExp.setText("");
                  txtNomeEXP.setText("");
                  
                  
             }
            
      }catch(Exception e){
                ehNumero = false;
                JOptionPane.showMessageDialog(rootPane, "CAMPO QUANTIDADE DE EXPERIMENTOS ACEITA SOMENTE NÚMEROS");
                txtEndExp.setText("");
                txtNomeEXP.setText("");
                txtQauntExp.setText("");
      }
            
            
            
   } else {
                  
       id_linhaTabelaExp = -1;
	   if (txtEndExp.getText().equals("")) {
              JOptionPane.showMessageDialog(rootPane, "INFORME O ENDEREÇO DO EXPERIMENTO");
              txtEndExp.requestFocus();
          } else if (txtNomeEXP.getText().equals("")) {
              JOptionPane.showMessageDialog(rootPane, "INFORME O NOME DO EXPERIMENTO");
              txtNomeEXP.requestFocus();
          } else if (id_linhaTabelaExp == -1) {
	   
        	contExp = modelo.getRowCount();
            String[] dados = new String[13];
            int idModel = jComboBoxRefModelExp.getSelectedIndex() + 1;

            dados[0] = "" + idModel;
            dados[1] = "" + jComboBoxRefModelExp.getSelectedItem();
            contExp = contExp + 1;
            dados[2] = "EXP0" + contExp;
            dados[3] = txtEndExp.getText();
            dados[4] = txtNomeEXP.getText();
            nomeExp.add(txtNomeEXP.getText());

            if ((contLinhaTabela > Integer.parseInt(txtQauntExp.getText())) && (id_linhaTabelaExp == -1)) {
                  JOptionPane.showMessageDialog(rootPane, "JA INSERIDO TODOS OS EXPERIMENTOS");
                  txtEndExp.setText("");
                  txtNomeEXP.setText("");
                    
                }
            else{
                  modelo.addRow(dados);
                  id_linhaTabelaExp = -1;
                  txtEndExp.setText("");
                  txtNomeEXP.setText("");
                                    
             }   
	   
       }
	   
  
        	
   }


}//GEN-LAST:event_btAdicionarActionPerformed

private void tabelaEXPMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_tabelaEXPMouseClicked
    DefaultTableModel modelo = (DefaultTableModel) tabelaEXP.getModel();
    int linha = tabelaEXP.getSelectedRow();
    String id = modelo.getValueAt(linha, 0).toString();

    txtEndExp.setText(modelo.getValueAt(linha, 3).toString());
    txtNomeEXP.setText(modelo.getValueAt(linha, 4).toString());
    String tabelaIdModelo = modelo.getValueAt(linha, 0).toString();
    int idModelo = Integer.parseInt(tabelaIdModelo) - 1;

    jComboBoxRefModelExp.setSelectedIndex(idModelo);
    id_linhaTabelaExp = linha;
}//GEN-LAST:event_tabelaEXPMouseClicked

private void btnDeletarLinhaActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnDeletarLinhaActionPerformed
// TODO add your handling code here:
    DefaultTableModel modeloTabelaRecorte = ((DefaultTableModel) tabelaRecorte.getModel());
    int linha = tabelaRecorte.getSelectedRow();
    
        
    	if(linha == -1) {
          JOptionPane.showMessageDialog(rootPane, "NENHUM RECORTE SELECIONADO");
        } 
    	
    	else{
    	String id = modeloTabelaRecorte.getValueAt(linha, 0).toString();
        modeloTabelaRecorte.removeRow(linha);
        listaRecorte.remove(linha);


        for (int i = 0; i < modeloTabelaRecorte.getRowCount(); i++) {
            modeloTabelaRecorte.setValueAt(i + 1, i, contExp);
        }   
    
        
    }
    
   
   
    
}//GEN-LAST:event_btnDeletarLinhaActionPerformed

private void btnAdicionarRecorteActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnAdicionarRecorteActionPerformed



    /*Deixar as datas no padrão do scamtec*/
    String dataInicialSemBarra = UtilData.dateToStringSemBarra(jDateDataIni.getDate());
    String dataFinalSemBarra = UtilData.dateToStringSemBarra(jDateDataFinal.getDate());
    /*----------------------------------------*/
    Recorte recorte = new Recorte(
            txtAnaliseTempo.getText(),
            txtPrevisaoTempo.getText(),
            txtPrevisaoTempoTotal.getText(),
            txtHistoriaTempo.getText(),
            dataInicialSemBarra,
            dataFinalSemBarra,
            jComboBoxHoraIni.getSelectedItem().toString(),
            jComboBoxHoraFinal.getSelectedItem().toString(),
            txtLatEsq.getText(),
            txtLatDireta.getText(),
            txtLongInf.getText(),
            txtLongSup.getText(),
            txtResolucaoX.getText(),
            txtResolucaoY.getText(),
            jComboBoxRegiao.getSelectedItem().toString());
    if (listaRecorte.size() < 10) {


        listaRecorte.add(recorte);
        DefaultTableModel modeloTabelaRecorte = ((DefaultTableModel) tabelaRecorte.getModel());
        modeloTabelaRecorte.addRow(new Object[]{
                    //listaRecorte.size() começa em 1, mas a posição inicial do vetor é 0
                    modeloTabelaRecorte.getRowCount() + 1,
                    listaRecorte.get(listaRecorte.size() - 1).getRegiao(),
                    listaRecorte.get(listaRecorte.size() - 1).getLatEsq(),
                    listaRecorte.get(listaRecorte.size() - 1).getLatDireta(),
                    listaRecorte.get(listaRecorte.size() - 1).getLongInf(),
                    listaRecorte.get(listaRecorte.size() - 1).getLongSup(),
                    listaRecorte.get(listaRecorte.size() - 1).getResolucaoX(),
                    listaRecorte.get(listaRecorte.size() - 1).getResolucaoY()
                });

    } else {
        JOptionPane.showMessageDialog(rootPane, "Você não pode colocar mais de 10 rodadas ao mesmo tempo");
    }
}//GEN-LAST:event_btnAdicionarRecorteActionPerformed

private void jComboBoxRegiaoActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxRegiaoActionPerformed
    /*
     * jComboBoxRegiao
     * 0 = AMERICA DO SUL
     * 1 = HEMISFERIO SUL
     * 2 = BRASIL
     * 3 = SÃO PAULO
     */

    if (jComboBoxRegiao.getSelectedIndex() == 0) {//AMERICA DO SUL
        txtLatEsq.setText("-49.875");
        txtLongInf.setText("-82.625");
        txtLatDireta.setText("11.375");
        txtLongSup.setText("-35.375");
        txtResolucaoX.setText("0.25");
        txtResolucaoY.setText("0.25");
        //jLabelMapa.setIcon(new javax.swing.ImageIcon(getClass().getResource("/br/scamtec/frame/America do SUL.jpg")));
    } else if (jComboBoxRegiao.getSelectedIndex() == 1) {//HEMISFERIO SUL
        txtLatEsq.setText("-80");
        txtLongInf.setText("0");
        txtLatDireta.setText("-20");
        txtLongSup.setText("360");
        txtResolucaoX.setText("0.4");
        txtResolucaoY.setText("0.4");
    } else if (jComboBoxRegiao.getSelectedIndex() == 2) {//BRASIL
        txtLatEsq.setText("-35");
        txtLongInf.setText("-80");
        txtLatDireta.setText("10");
        txtLongSup.setText("-30");
        txtResolucaoX.setText("0.4");
        txtResolucaoY.setText("0.4");
    } else if (jComboBoxRegiao.getSelectedIndex() == 3) {// SAO PAULO
        txtLatEsq.setText("-27");
        txtLongInf.setText("-55");
        txtLatDireta.setText("-20");
        txtLongSup.setText("-40");
        txtResolucaoX.setText("0.4");
        txtResolucaoY.setText("0.4");
        //jLabelMapa.setIcon(new javax.swing.ImageIcon(getClass().getResource("/br/scamtec/frame/mapa-rodoviario-sao-paulo.jpg")));
    } else if (jComboBoxRegiao.getSelectedItem().toString() == "NOVO") {//NOVO
        txtLatEsq.setText("");
        txtLongInf.setText("");
        txtLatDireta.setText("");
        txtLongSup.setText("");
        txtResolucaoX.setText("");
        txtResolucaoY.setText("");

    }
}//GEN-LAST:event_jComboBoxRegiaoActionPerformed

/* Comentando o botao carregar arquivo ja pronto.
private void btLoaderScamConfActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btLoaderScamConfActionPerformed
    JFileChooser file = new JFileChooser();
    file.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int i = file.showSaveDialog(null);
    if (i == 1) {
        endArq = "";
        JOptionPane.showMessageDialog(rootPane, "Arquivo inválido");
    } else {
        //abrindo arquivo
        File arquivo = file.getSelectedFile();
        endArq = arquivo.getPath();
        procura(endArq);
        //pegando o nome de cada experimento
        DefaultTableModel modelo = (DefaultTableModel) tabelaEXP.getModel();
        int quantExp = Integer.parseInt(txtQauntExp.getText());
        for (int j = 1; j < quantExp + 1; j++) {
            nomeExp.add(modelo.getValueAt(j - 1, 4).toString());
        }

    }
}//GEN-LAST:event_btLoaderScamConfActionPerformed
*/


private void txtHistoriaTempoActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_txtHistoriaTempoActionPerformed
    int teste = difDatas();
}//GEN-LAST:event_txtHistoriaTempoActionPerformed

private void geraGraficoGeralEmHtmlActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_geraGraficoGeralEmHtmlActionPerformed
// TODO add your handling code here:
    GraficoComparativo grafico = new GraficoComparativo();
    try {
        /*java.io.File dirOrigem = new java.io.File("/home/rafael/Público/a/");  
        java.io.File dirDestino = new java.io.File("/home/rafael/Público/b/");
        grafico.copiarPastaDeImagens(dirOrigem, dirDestino, true);*/
        //grafico.copyDirectory(new File("./images/"),new File("/home/rafael/pastateste/"));
        grafico.GerarHtmlComGraficoComparativo(
                txtACOR1.getText(),
                txtACOR2.getText(),
                txtRMS1.getText(),
                txtRMS2.getText(),
                txtVIES1.getText(),
                txtVIES2.getText(),
                txtEndSaida.getText(),
                jComboBoxRecorte.getSelectedItem().toString());
        /*grafico.GerarHtmlComGraficoComparativo(
                "/home/rafael/IMPACTO_scamtec/psasxtag_AS/ACOREXP01_20130101002013012612.scam",
                "/home/rafael/IMPACTO_scamtec/psasxtag_AS/ACOREXP02_20130101002013012612.scam",
                "/home/rafael/IMPACTO_scamtec/psasxtag_AS/RMSEEXP01_20130101002013012612.scam",
                "/home/rafael/IMPACTO_scamtec/psasxtag_AS/RMSEEXP02_20130101002013012612.scam",
                "/home/rafael/IMPACTO_scamtec/psasxtag_AS/VIESEXP01_20130101002013012612.scam",
                "/home/rafael/IMPACTO_scamtec/psasxtag_AS/VIESEXP02_20130101002013012612.scam",
                txtEndSaida.getText(),
                jComboBoxRecorte.getSelectedItem().toString());*/
        

    } catch (Exception ex) {
        System.out.println(ex.toString());
        ex.printStackTrace();
    }

}//GEN-LAST:event_geraGraficoGeralEmHtmlActionPerformed

private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed

 try {	 
	//botao Gerar todas as figuras
	if (jComboBoxVariavel.getSelectedItem().equals("Item 1") || jComboBoxVariavel.getSelectedItem().equals("Item 2") || jComboBoxVariavel.getSelectedItem().equals("Item 3") || jComboBoxVariavel.getSelectedItem().equals("Item 4") ) {
        JOptionPane.showMessageDialog(rootPane, "FAVOR CARREGAR FIGURAS CORRETAMENTE");
    } else if(jComboBoxRecorte.getSelectedItem().toString().contains("Recorte")){
        JOptionPane.showMessageDialog(rootPane, "Defina outro nome para região do Recorte!");
    } else if(txtEndSaida.getText().equals("")) {
        JOptionPane.showMessageDialog(rootPane, "INFORME O ENDEREÇO DOS RESULTADOS CORRETAMENTE");
        txtEndSaida.requestFocus();
    	    	
    }else{
        salvaGraficosAll salva = new salvaGraficosAll();
        Thread theradExe = new Thread(salva);
        theradExe.start();
        new Thread(new Carregar()).start();

    }
	
  } catch (Exception ex) {
     System.out.println(ex.toString());
     
 }
	
}//GEN-LAST:event_jButton1ActionPerformed




private void btLoadArqSaidaActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btLoadArqSaidaActionPerformed
    
			
	try {
                  
    	if(txtEndSaida.getText().equals("")) {
            JOptionPane.showMessageDialog(rootPane, "INFORME O ENDEREÇO DOS RESULTADOS CORRETAMENTE");
            txtEndSaida.requestFocus();
        	    	
        }else{
        	        	
        	leituraArqSaida();
        	
        }
    }  catch (Exception ex) {
    	System.out.println(ex.toString());
    	
    }
	
		
	
	
	
}//GEN-LAST:event_btLoadArqSaidaActionPerformed

private void jRadioButtonRmseMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jRadioButtonRmseMouseClicked
    leituraArqSaida();
}//GEN-LAST:event_jRadioButtonRmseMouseClicked

private void jRadioButtonViesMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jRadioButtonViesMouseClicked
    leituraArqSaida();
}//GEN-LAST:event_jRadioButtonViesMouseClicked

private void jComboBoxVariavelItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_jComboBoxVariavelItemStateChanged
    //leituraArqSaida();
    DefaultTableModel modelo = ((DefaultTableModel) tabelaValores.getModel());


    modelo.setNumRows(0);
    tabelaValores.updateUI();
    int totalPrev = Integer.parseInt(txtPrevisaoTempoTotal.getText());
    int horaPrev = Integer.parseInt(txtPrevisaoTempo.getText());
    int quatHoras = (totalPrev / horaPrev) + 2;//porque + 2 para somar a zero horas e o cabeçalho
    //System.out.println("quatHoras= "+quatHoras);
    String[] dados = new String[quatHoras];
    for (int w = 0; w < qauntExp; w++) { //loop quantidade de arquivos
        for (int j = 1; j < quatHoras; j++) {
            if (jRadioButtonAcor.isSelected() == true) {
                float valor = Float.parseFloat(valores[w][j][jComboBoxVariavel.getSelectedIndex() + 1]) * 100;
                dados[j - 1] = "" + valor;
            } else {
                dados[j - 1] = valores[w][j][jComboBoxVariavel.getSelectedIndex() + 1];
            }


        }
        modelo.addRow(dados);
    }
}//GEN-LAST:event_jComboBoxVariavelItemStateChanged

private void jRadioButtonAcorMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jRadioButtonAcorMouseClicked
    leituraArqSaida();
}//GEN-LAST:event_jRadioButtonAcorMouseClicked

private void btGraficoActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btGraficoActionPerformed
    try {
        //leituraArqSaida();          
    	if(txtEndSaida.getText().equals("")) {
            JOptionPane.showMessageDialog(rootPane, "INFORME O ENDEREÇO DOS RESULTADOS CORRETAMENTE");
            txtEndSaida.requestFocus();
        	    	
        }else{
        	        	
          PlotTest();
        	
        }
    } catch (FileNotFoundException ex) {
        Logger.getLogger(ScamtecConfiguracao.class.getName()).log(Level.SEVERE, null, ex);
    } catch (IOException ex) {
        Logger.getLogger(ScamtecConfiguracao.class.getName()).log(Level.SEVERE, null, ex);
    }
}//GEN-LAST:event_btGraficoActionPerformed

private void jComboBoxRecorteActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxRecorteActionPerformed
// TODO add your handling code here:
}//GEN-LAST:event_jComboBoxRecorteActionPerformed

private void txtVIES2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_txtVIES2ActionPerformed
// TODO add your handling code here:
}//GEN-LAST:event_txtVIES2ActionPerformed

private void txtRMS1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_txtRMS1ActionPerformed
// TODO add your handling code here:
}//GEN-LAST:event_txtRMS1ActionPerformed

private void txtACOR2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_txtACOR2ActionPerformed
// TODO add your handling code here:
}//GEN-LAST:event_txtACOR2ActionPerformed
    /*
     *  Autor:Rafael
     *  Funcionalidade: Função para adicionar as váriaveis
     *                  do recorte em uma lista de arrays
     *                  para depois fazer vários testes automaticamente
     *///--------------------------------------------------------------
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JProgressBar barraProcesso;
    private javax.swing.JButton btAdicionar;
    private javax.swing.JButton btDeletar;
    private javax.swing.JButton btGrafico;
    private javax.swing.JButton btLoadArqSaida;
    //private javax.swing.JButton btLoaderScamConf;
    private javax.swing.JButton btRun;
    private javax.swing.JButton btnAdicionarRecorte;
    private javax.swing.JButton btnDeletarLinha;
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton geraGraficoGeralEmHtml;
    private javax.swing.JButton jButton1;
    private javax.swing.JComboBox jComboBoxHoraFinal;
    private javax.swing.JComboBox jComboBoxHoraIni;
    private javax.swing.JComboBox jComboBoxRecorte;
    private javax.swing.JComboBox jComboBoxRefModelAnlise;
    private javax.swing.JComboBox jComboBoxRefModelExp;
    private javax.swing.JComboBox jComboBoxRefModelxClima;
    private javax.swing.JComboBox jComboBoxRefModelxPrecip;
    private javax.swing.JComboBox jComboBoxRegiao;
    private javax.swing.JComboBox jComboBoxVariavel;
    private com.toedter.calendar.JDateChooser jDateDataFinal;
    private com.toedter.calendar.JDateChooser jDateDataIni;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel14;
    private javax.swing.JLabel jLabel15;
    private javax.swing.JLabel jLabel16;
    private javax.swing.JLabel jLabel17;
    private javax.swing.JLabel jLabel18;
    private javax.swing.JLabel jLabel19;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel20;
    private javax.swing.JLabel jLabel21;
    private javax.swing.JLabel jLabel22;
    private javax.swing.JLabel jLabel23;
    private javax.swing.JLabel jLabel24;
    private javax.swing.JLabel jLabel25;
    private javax.swing.JLabel jLabel26;
    private javax.swing.JLabel jLabel27;
    private javax.swing.JLabel jLabel28;
    private javax.swing.JLabel jLabel29;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel30;
    private javax.swing.JLabel jLabel31;
    private javax.swing.JLabel jLabel32;
    private javax.swing.JLabel jLabel33;
    private javax.swing.JLabel jLabel34;
    private javax.swing.JLabel jLabel35;
    private javax.swing.JLabel jLabel36;
    private javax.swing.JLabel jLabel37;
    private javax.swing.JLabel jLabel38;
    private javax.swing.JLabel jLabel39;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel40;
    private javax.swing.JLabel jLabel41;
    private javax.swing.JLabel jLabel42;
    private javax.swing.JLabel jLabel43;
    private javax.swing.JLabel jLabel44;
    private javax.swing.JLabel jLabel45;
    private javax.swing.JLabel jLabel46;
    private javax.swing.JLabel jLabel47;
    private javax.swing.JLabel jLabel48;
    private javax.swing.JLabel jLabel49;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel50;
    private javax.swing.JLabel jLabel51;
    private javax.swing.JLabel jLabel52;
    private javax.swing.JLabel jLabel53;
    private javax.swing.JLabel jLabel54;
    private javax.swing.JLabel jLabel55;
    private javax.swing.JLabel jLabel56;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel10;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JPanel jPanel12;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JPanel jPanel9;
    private javax.swing.JPanel jPanelGrafico;
    private javax.swing.JPanel jPanelPropArq;
    private javax.swing.JPanel jPanelPropOpcao;
    private javax.swing.JPanel jPanelRun;
    private javax.swing.JPanel jPanelTempoDominio;
    private javax.swing.JRadioButton jRadioButtonAcor;
    private javax.swing.JRadioButton jRadioButtonRmse;
    private javax.swing.JRadioButton jRadioButtonUseClima;
    private javax.swing.JRadioButton jRadioButtonUsePrecip;
    private javax.swing.JRadioButton jRadioButtonVies;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JScrollPane jScrollPane4;
    private javax.swing.JTabbedPane jTabbedPaneGrafico;
    private javax.swing.JTable tabelaEXP;
    private javax.swing.JTable tabelaRecorte;
    private javax.swing.JTable tabelaValores;
    private javax.swing.JTextField txtACOR1;
    private javax.swing.JTextField txtACOR2;
    private javax.swing.JTextField txtAnaliseTempo;
    private javax.swing.JTextField txtArqSaida;
    private javax.swing.JTextField txtEndAnalise;
    private javax.swing.JTextField txtEndClima;
    private javax.swing.JTextField txtEndExecutavel;
    private javax.swing.JTextField txtEndExp;
    private javax.swing.JTextField txtEndPrecip;
    private javax.swing.JTextField txtEndSaida;
    private javax.swing.JTextField txtHistoriaTempo;
    private javax.swing.JTextField txtLatDireta;
    private javax.swing.JTextField txtLatEsq;
    private javax.swing.JTextField txtLongInf;
    private javax.swing.JTextField txtLongSup;
    private javax.swing.JTextField txtNomeEXP;
    private javax.swing.JTextField txtPrecipAcumuloExp;
    private javax.swing.JTextField txtPrecipAcumuloObs;
    private javax.swing.JTextField txtPrecipLimite;
    private javax.swing.JTextField txtPrecipMinimo;
    private javax.swing.JTextField txtPrecipRange;
    private javax.swing.JTextField txtPrecipTipo;
    private javax.swing.JTextField txtPrevisaoTempo;
    private javax.swing.JTextField txtPrevisaoTempoTotal;
    private javax.swing.JTextField txtQauntExp;
    private javax.swing.JTextField txtRMS1;
    private javax.swing.JTextField txtRMS2;
    private javax.swing.JTextField txtResolucaoX;
    private javax.swing.JTextField txtResolucaoY;
    private javax.swing.JTextArea txtResult;
    private javax.swing.JTextField txtVIES1;
    private javax.swing.JTextField txtVIES2;
    // End of variables declaration//GEN-END:variables
}
