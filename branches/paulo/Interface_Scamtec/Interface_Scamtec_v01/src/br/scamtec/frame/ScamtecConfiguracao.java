/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package br.scamtec.frame;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import javax.swing.JOptionPane;
import javax.swing.table.DefaultTableModel;
import java.util.Date;
import br.scamtec.classes.UtilData;
import java.io.FileReader;
import java.util.GregorianCalendar;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JFileChooser;

/**
 *
 * @author paulo.henrique
 */
public class ScamtecConfiguracao extends javax.swing.JInternalFrame {

    int contExp = 0;
    int useClima = 1;
    int usePrecip = 0;
    String endArq = "";

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
    }

    private void tabelaPadrao() {

        DefaultTableModel modelo = ((DefaultTableModel) tabelaEXP.getModel());
        modelo.setRowCount(0);
        modelo.setColumnCount(0);

        modelo.addColumn("ID MODELO");
        modelo.addColumn("MODELO");
        modelo.addColumn("EXP");
        modelo.addColumn("ENDEREÇO");


        tabelaEXP.getColumnModel().getColumn(0).setPreferredWidth(10);
        tabelaEXP.getColumnModel().getColumn(1).setPreferredWidth(20);
        tabelaEXP.getColumnModel().getColumn(2).setPreferredWidth(10);
        tabelaEXP.getColumnModel().getColumn(3).setPreferredWidth(1000);
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
        txtPrevisaoTempoTotal.setText("120");

    }

    public class ExecutaScamtec implements Runnable {

        @Override
        public void run() {
            try {
                String line;
                String line2 = "";
                txtResult.setText("");
                System.out.println("teste "+txtEndExecutavel.getText() + "scamtec.x");
                Process p = Runtime.getRuntime().exec(txtEndExecutavel.getText() + "scamtec.x");
                
                try (BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()))) {
                    int cont = 0;
                    jProgressBar1.setMaximum(difDatas());
                    System.out.println("HELLO " + difDatas());

                    while ((line = input.readLine()) != null) {

                        line2 = line2 + line + "\n";
                        cont = cont + 1;
                        jProgressBar1.setValue(cont);
                        txtResult.setText(line2);
                        txtResult.selectAll();
                        System.out.println(line);
                        // jProgressBar1.setValue(p.waitFor());
                    }
                }
            } catch (Exception err) {
                err.printStackTrace();
            }

        }
    }

    private int difDatas() {

        Date dataA = UtilData.getDataCalendario(jDateDataIni.getDate());
        Date dataVenc = UtilData.getDataCalendario(jDateDataFinal.getDate());

        System.out.print("data " + dataA);

        int horaPrev = Integer.parseInt(txtPrevisaoTempo.getText());

        int horaIni = Integer.parseInt("" + jComboBoxHoraIni.getSelectedItem());
        int horaFinal = Integer.parseInt("" + jComboBoxHoraFinal.getSelectedItem());
        int difHora = horaFinal - horaIni;

        int horaPrevTotal = Integer.parseInt(txtPrevisaoTempoTotal.getText());

        int quantPrev = (horaPrevTotal / horaPrev) + 1;

        int difData = dataDiff(dataA, dataVenc);
        //if (vencimento == 0) {
        //   vencimento = 1;
        //}


        //int tempo=24/difHora;



        int total = 0;
        System.out.println("\nHELLO2 " + difData + " " + difHora + ' ' + (horaPrevTotal / horaPrev));

        total = difData + difHora + quantPrev + 3;
        System.out.println("\nHELLO " + difData + " " + total + ' ' + difHora);
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

    public class Carregar implements Runnable {

        @Override
        public void run() {
            for (int i = 0; i <= 100; i++) {
                jProgressBar1.setValue(i);
                try {
                    Thread.sleep(50);

                } catch (InterruptedException ex) {
                    Logger.getLogger(ScamtecConfiguracao.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
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
                BufferedReader in = new BufferedReader(new FileReader(arq));
                while ((linha = in.readLine()) != null) {
                    if (linha.contains("EXP0" + i)) {
                        vetor = linha.split(" ");
                        // System.out.println(linha);
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

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jTabbedPane1 = new javax.swing.JTabbedPane();
        jPanel1 = new javax.swing.JPanel();
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
        jButton1 = new javax.swing.JButton();
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
        jPanel2 = new javax.swing.JPanel();
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
        jPanel7 = new javax.swing.JPanel();
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
        btSalvar = new javax.swing.JButton();
        jPanel11 = new javax.swing.JPanel();
        jLabel42 = new javax.swing.JLabel();
        txtEndExecutavel = new javax.swing.JTextField();
        jLabel43 = new javax.swing.JLabel();
        jProgressBar1 = new javax.swing.JProgressBar();
        jPanel12 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        txtResult = new javax.swing.JTextArea();

        setClosable(true);
        setTitle("SCAMTEC CONFIGURAÇÕES");

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "PROPRIEDADE DO TEMPO", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 12), java.awt.Color.blue)); // NOI18N

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

        jButton1.setIcon(new javax.swing.ImageIcon(getClass().getResource("/br/scamtec/imagens/text.png"))); // NOI18N
        jButton1.setText("LOADER");
        jButton1.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton1.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel2)
                    .addComponent(jLabel1))
                .addGap(4, 4, 4)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jDateDataIni, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.PREFERRED_SIZE, 130, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jDateDataFinal, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.PREFERRED_SIZE, 130, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jComboBoxHoraIni, javax.swing.GroupLayout.PREFERRED_SIZE, 52, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jComboBoxHoraFinal, javax.swing.GroupLayout.PREFERRED_SIZE, 52, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(4, 4, 4)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel44)
                    .addComponent(jLabel41))
                .addGap(18, 18, 18)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel3, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel4, javax.swing.GroupLayout.Alignment.TRAILING))
                .addGap(3, 3, 3)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(txtAnaliseTempo, javax.swing.GroupLayout.PREFERRED_SIZE, 39, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(txtPrevisaoTempo, javax.swing.GroupLayout.PREFERRED_SIZE, 39, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(4, 4, 4)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(jLabel48)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jLabel6))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(jLabel45)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel5)))
                .addGap(4, 4, 4)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(txtPrevisaoTempoTotal, javax.swing.GroupLayout.PREFERRED_SIZE, 39, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(txtHistoriaTempo, javax.swing.GroupLayout.PREFERRED_SIZE, 39, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(4, 4, 4)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel47)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(jLabel46)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButton1)))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addGap(15, 15, 15)
                        .addComponent(jLabel2))
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(jDateDataIni, javax.swing.GroupLayout.PREFERRED_SIZE, 24, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(6, 6, 6)
                        .addComponent(jDateDataFinal, javax.swing.GroupLayout.PREFERRED_SIZE, 24, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(jComboBoxHoraIni, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(6, 6, 6)
                        .addComponent(jComboBoxHoraFinal, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(jLabel41)
                        .addGap(15, 15, 15)
                        .addComponent(jLabel44))
                    .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(jButton1)
                        .addGroup(jPanel3Layout.createSequentialGroup()
                            .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                .addComponent(jLabel3)
                                .addComponent(txtAnaliseTempo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addComponent(jLabel5)
                                .addComponent(txtPrevisaoTempoTotal, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addComponent(jLabel45)
                                .addComponent(jLabel46))
                            .addGap(11, 11, 11)
                            .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                .addComponent(jLabel4)
                                .addComponent(txtPrevisaoTempo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addComponent(jLabel6)
                                .addComponent(txtHistoriaTempo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addComponent(jLabel47)
                                .addComponent(jLabel48)))))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel4.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "PROPRIEDADES DE RESOLUÇÃO", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 12), java.awt.Color.blue)); // NOI18N

        jLabel7.setText("LATITUDE ESQUERDA:");

        jLabel12.setText("RESOLUÇÃO DY:");

        jLabel11.setText("RESOLUÇÃO DX:");

        jLabel8.setText("LATITUDE DIREITA:");

        jLabel9.setText("LONGITUDE INFERIOR:");

        jLabel10.setText("LONGITUDE SUPERIOR:");

        jComboBoxRegiao.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "AMERICA DO SUL", "HEMISFERIO SUL", "BRASIL", "SÃO PAULO", "NOVO" }));
        jComboBoxRegiao.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxRegiaoActionPerformed(evt);
            }
        });

        jLabel16.setText("REGIÕES PRÉ CONFIGURADO");

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel7, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel8, javax.swing.GroupLayout.Alignment.TRAILING))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
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
                    .addComponent(jComboBoxRegiao, javax.swing.GroupLayout.PREFERRED_SIZE, 158, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(650, Short.MAX_VALUE))
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
                    .addComponent(jLabel8)
                    .addComponent(txtLatDireta, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel10)
                    .addComponent(txtLongSup, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel12)
                    .addComponent(txtResolucaoY, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jComboBoxRegiao, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(585, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("PROPRIEDADES DO TEMPO E DOMINIO", jPanel1);

        jPanel5.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "PROPRIEDADES DO ARQUIVO DE ANALISE", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 12), java.awt.Color.blue)); // NOI18N

        jComboBoxRefModelAnlise.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "AGCM/CPTEC", "ETA/CPTEC", "50yr Climatology", "IWV-PSAS T126", "Precipitation", " " }));

        jLabel13.setText("SELECIONE O MODELO DE ANALISE");

        jLabel14.setText("ENDEREÇO:");

        jLabel15.setText("/stornext/online6/assim_dados/paulo.dias/pos_G3DVAR_GPS_C_SAT/TQ0299L064/%y4%m2%d2%h2/GPOSNMC%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2P.fct.TQ0299L064.grb");

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
                    .addComponent(jLabel15, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(txtEndAnalise))
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

        jPanel6.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "PROPRIEDADES DOS ARQUIVOS DO EXPERIMENTO", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 12), java.awt.Color.blue)); // NOI18N

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

        jComboBoxRefModelExp.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "AGCM/CPTEC", "ETA/CPTEC", "50yr Climatology", "IWV-PSAS T126", "Precipitation", " " }));

        jLabel19.setText("ENDEREÇO:");

        jLabel20.setText("/stornext/online6/assim_dados/paulo.dias/pos_G3DVAR_GPS_C_SAT/TQ0299L064/%y4%m2%d2%h2/GPOSNMC%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2P.fct.TQ0299L064.grb");

        btAdicionar.setText("ADCIONAR");
        btAdicionar.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btAdicionarActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane2)
                    .addComponent(jLabel18, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanel6Layout.createSequentialGroup()
                        .addComponent(jComboBoxRefModelExp, javax.swing.GroupLayout.PREFERRED_SIZE, 276, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel19)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel20, javax.swing.GroupLayout.DEFAULT_SIZE, 1194, Short.MAX_VALUE)
                            .addComponent(txtEndExp)))
                    .addGroup(jPanel6Layout.createSequentialGroup()
                        .addComponent(jLabel17)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(txtQauntExp, javax.swing.GroupLayout.PREFERRED_SIZE, 51, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(btAdicionar)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel6Layout.setVerticalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel17)
                    .addComponent(txtQauntExp, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btAdicionar))
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
                .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 540, Short.MAX_VALUE)
                .addContainerGap())
        );

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel5, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("PROPRIEDADES DOS ARQUIVOS", jPanel2);

        jPanel8.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "ARQUIVO DE CLIMATOLOGIA", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 12), java.awt.Color.blue)); // NOI18N

        jRadioButtonUseClima.setText("USE CLIMATOLOGIA");
        jRadioButtonUseClima.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jRadioButtonUseClimaMouseClicked(evt);
            }
        });

        jLabel21.setText("MODELO DE CLIMATOLOGIA");

        jComboBoxRefModelxClima.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "50yr Climatology" }));
        jComboBoxRefModelxClima.setFocusable(false);

        jLabel22.setText("ENDEREÇO:");

        jLabel23.setText("/stornext/online6/assim_dados/paulo.dias/Dados_SCAMTEC/climatologiaJGM/climatologia50yr.%mc.bin");

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
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addComponent(txtEndClima))
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

        jPanel9.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "ARQUIVO DE PRECIPITAÇÃO", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 12), java.awt.Color.blue)); // NOI18N

        jRadioButtonUsePrecip.setText("USE PRECIPITAÇÃO");
        jRadioButtonUsePrecip.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jRadioButtonUsePrecipMouseClicked(evt);
            }
        });

        jLabel24.setText("MODELO DE CLIMATOLOGIA");

        jComboBoxRefModelxPrecip.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Precipitation" }));
        jComboBoxRefModelxPrecip.setFocusable(false);

        jLabel25.setText("ENDEREÇO:");

        jLabel26.setText("/stornext/online6/assim_dados/paulo.dias/Dados_SCAMTEC/preciptacao/%y4/3B42RT_SA.%y4%m2%d2%h200.bin");

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
                .addGap(0, 0, Short.MAX_VALUE))
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
                                .addGap(0, 489, Short.MAX_VALUE))
                            .addComponent(txtEndPrecip)))
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

        jPanel10.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "DIRETORIO DE SAÍDA", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 12), java.awt.Color.blue)); // NOI18N

        jLabel39.setText("ENDEREÇO:");

        jLabel40.setText("/stornext/home/paulo.dias/SCAMTEC_Paulo_New_Variaveis/SCAMTEC_v0.1/core/results/");

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
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addComponent(txtEndSaida))
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

        btSalvar.setText("SALVAR");
        btSalvar.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btSalvarActionPerformed(evt);
            }
        });

        jPanel11.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "DIRETORIO DO SCAMTEC.X", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 12), java.awt.Color.blue)); // NOI18N

        jLabel42.setText("ENDEREÇO:");

        jLabel43.setText("/stornext/home/paulo.dias/SCAMTEC_Paulo_New_Variaveis/SCAMTEC_v0.1/core/");

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
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addComponent(txtEndExecutavel))
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

        jProgressBar1.setStringPainted(true);

        jPanel12.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "RESULTADO", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 12), java.awt.Color.blue)); // NOI18N

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
                .addComponent(jScrollPane1)
                .addContainerGap())
        );
        jPanel12Layout.setVerticalGroup(
            jPanel12Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel12Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 206, Short.MAX_VALUE)
                .addContainerGap())
        );

        javax.swing.GroupLayout jPanel7Layout = new javax.swing.GroupLayout(jPanel7);
        jPanel7.setLayout(jPanel7Layout);
        jPanel7Layout.setHorizontalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel8, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel9, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel10, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel7Layout.createSequentialGroup()
                        .addComponent(jProgressBar1, javax.swing.GroupLayout.PREFERRED_SIZE, 396, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(btSalvar))
                    .addComponent(jPanel11, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel12, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel7Layout.setVerticalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel8, javax.swing.GroupLayout.PREFERRED_SIZE, 102, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel9, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel10, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel11, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel12, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btSalvar)
                    .addComponent(jProgressBar1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        jTabbedPane1.addTab("PROPRIEDADES DE OPÇÃO", jPanel7);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jTabbedPane1)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jTabbedPane1)
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

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
        } else if (jComboBoxRegiao.getSelectedIndex() == 4) {//NOVO
            txtLatEsq.setText("");
            txtLongInf.setText("");
            txtLatDireta.setText("");
            txtLongSup.setText("");
            txtResolucaoX.setText("");
            txtResolucaoY.setText("");
        }
    }//GEN-LAST:event_jComboBoxRegiaoActionPerformed

    private void tabelaEXPMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_tabelaEXPMouseClicked
        DefaultTableModel modelo = (DefaultTableModel) tabelaEXP.getModel();
        int linha = tabelaEXP.getSelectedRow();
        String id = modelo.getValueAt(linha, 0).toString();

        txtEndExp.setText(modelo.getValueAt(linha, 3).toString());

        String tabelaIdModelo = modelo.getValueAt(linha, 0).toString();
        int idModelo = Integer.parseInt(tabelaIdModelo) - 1;

        jComboBoxRefModelExp.setSelectedIndex(idModelo);
    }//GEN-LAST:event_tabelaEXPMouseClicked

    private void btAdicionarActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btAdicionarActionPerformed
        DefaultTableModel modelo = ((DefaultTableModel) tabelaEXP.getModel());

        int contLinhaTabela = modelo.getRowCount() + 1;
        if (contLinhaTabela > Integer.parseInt(txtQauntExp.getText())) {
            JOptionPane.showMessageDialog(rootPane, "QUANTIDADE NÃO PERMITIDA!!!");
            txtEndExp.setText("");
        } else {
            String[] dados = new String[13];
            int idModel = jComboBoxRefModelExp.getSelectedIndex() + 1;

            dados[0] = "" + idModel;
            dados[1] = "" + jComboBoxRefModelExp.getSelectedItem();
            contExp = contExp + 1;
            dados[2] = "EXP0" + contExp;
            dados[3] = txtEndExp.getText();

            modelo.addRow(dados);
            txtEndExp.setText("");
        }
    }//GEN-LAST:event_btAdicionarActionPerformed

    private void jRadioButtonUseClimaMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jRadioButtonUseClimaMouseClicked
        if (jRadioButtonUseClima.isSelected() == true) {
            useClima = 1;

        } else {
            useClima = 0;
        }
    }//GEN-LAST:event_jRadioButtonUseClimaMouseClicked

    private void jRadioButtonUsePrecipMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jRadioButtonUsePrecipMouseClicked
        if (jRadioButtonUsePrecip.isSelected() == true) {
            txtEndPrecip.setEditable(true);
            usePrecip = 1;

        } else {
            txtEndClima.setEditable(false);
            usePrecip = 0;
        }
    }//GEN-LAST:event_jRadioButtonUsePrecipMouseClicked

    private void btSalvarActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btSalvarActionPerformed
        DefaultTableModel modelo = (DefaultTableModel) tabelaEXP.getModel();

        if (jDateDataIni.getDate() == null) {
            JOptionPane.showMessageDialog(rootPane, "INFORME A DATA INICIAL");
            jTabbedPane1.setSelectedIndex(0);
            jDateDataIni.requestFocus();
        } else if (jDateDataFinal.getDate() == null) {
            JOptionPane.showMessageDialog(rootPane, "INFORME A DATA FINAL");
            jTabbedPane1.setSelectedIndex(0);
            jDateDataFinal.requestFocus();
        } else if (txtEndAnalise.getText().equals("")) {
            JOptionPane.showMessageDialog(rootPane, "INFORME O ENDEREÇO DO ARQUIVO DE ANALISE");
            jTabbedPane1.setSelectedIndex(1);
            txtEndAnalise.requestFocus();
        } else if (txtQauntExp.getText().equals("")) {
            JOptionPane.showMessageDialog(rootPane, "INFORME A QUANTIDADE DE EXPERIMENTO");
            jTabbedPane1.setSelectedIndex(1);
            txtQauntExp.requestFocus();
        } else if (modelo.getRowCount() == 0) {
            JOptionPane.showMessageDialog(rootPane, "INFORME O ARQUIVO DE EXPERIMENTO");
            jTabbedPane1.setSelectedIndex(1);
            txtEndExp.requestFocus();
        } else if (jRadioButtonUseClima.isSelected() == true && txtEndClima.getText().equals("")) {
            JOptionPane.showMessageDialog(rootPane, "INFORME O ARQUIVO DE CLIMATOLOGIA");
            jTabbedPane1.setSelectedIndex(2);
            txtEndClima.requestFocus();
        } else if (jRadioButtonUsePrecip.isSelected() == true && txtEndPrecip.getText().equals("")) {
            JOptionPane.showMessageDialog(rootPane, "INFORME O ARQUIVO DE PRECIPITAÇÃO");
            jTabbedPane1.setSelectedIndex(2);
            txtEndPrecip.requestFocus();
        } else if (txtEndSaida.getText().equals("")) {
            JOptionPane.showMessageDialog(rootPane, "INFORME O ENDEREÇO DE SAÍDA");
            jTabbedPane1.setSelectedIndex(2);
            txtEndSaida.requestFocus();
        } else if (txtEndExecutavel.getText().equals("")) {
            JOptionPane.showMessageDialog(rootPane, "INFORME O ENDEREÇO DO EXECUTÁVEL");
            jTabbedPane1.setSelectedIndex(2);
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
                Date dataI = jDateDataIni.getDate();
                String dataIni = UtilData.dateToStringSemBarra(dataI) + jComboBoxHoraIni.getSelectedItem();
                texto = "\nStarting Time: " + dataIni + " #Format  :: YYYYMMDDHH";
                fos.write(texto.getBytes());

                Date dataF = jDateDataFinal.getDate();
                String dataFinal = UtilData.dateToStringSemBarra(dataF) + jComboBoxHoraFinal.getSelectedItem();
                texto = "\nEnding Time: " + dataFinal + " #Format  :: YYYYMMDDHH";
                fos.write(texto.getBytes());

                texto = "\nAnalisys Time Step: " + txtAnaliseTempo.getText() + " #Format  :: HH";
                fos.write(texto.getBytes());

                texto = "\nForecast Time Step: " + txtPrevisaoTempo.getText() + " #Format  :: HH";
                fos.write(texto.getBytes());

                texto = "\nForecast Total Time: " + txtPrevisaoTempoTotal.getText() + " #Format  :: HH";
                fos.write(texto.getBytes());

                texto = "\nHistory Time: " + txtHistoriaTempo.getText() + " #Format  :: HH";
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
                texto = "\nrun domain lower left lat: " + txtLatEsq.getText();
                fos.write(texto.getBytes());
                texto = "\nrun domain lower left lon: " + txtLongInf.getText();
                fos.write(texto.getBytes());
                texto = "\nrun domain upper right lat: " + txtLatDireta.getText();
                fos.write(texto.getBytes());
                texto = "\nrun domain upper right lon: " + txtLongSup.getText();
                fos.write(texto.getBytes());
                texto = "\nrun domain resolution dx: " + txtResolucaoX.getText();
                fos.write(texto.getBytes());
                texto = "\nrun domain resolution dy: " + txtResolucaoY.getText();
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
                texto = "\n\nOutput directory: " + txtEndSaida.getText();
                fos.write(texto.getBytes());

                fos.close();

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



                // JOptionPane.showMessageDialog(rootPane, "CONFIGURACAO REALIZADA COM SUCESSO");


            } catch (Exception ee) {
                ee.printStackTrace();
            }
        }


    }//GEN-LAST:event_btSalvarActionPerformed

    private void txtHistoriaTempoActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_txtHistoriaTempoActionPerformed
        int teste = difDatas();
    }//GEN-LAST:event_txtHistoriaTempoActionPerformed

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        JFileChooser file = new JFileChooser();
        file.setFileSelectionMode(JFileChooser.FILES_ONLY);
        int i = file.showSaveDialog(null);
        if (i == 1) {
            endArq = "";
            JOptionPane.showMessageDialog(rootPane, "Arquivo inválido");
        } else {
            File arquivo = file.getSelectedFile();
            endArq = arquivo.getPath();
            procura(endArq);

        }
    }//GEN-LAST:event_jButton1ActionPerformed
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btAdicionar;
    private javax.swing.JButton btSalvar;
    private javax.swing.JButton jButton1;
    private javax.swing.JComboBox jComboBoxHoraFinal;
    private javax.swing.JComboBox jComboBoxHoraIni;
    private javax.swing.JComboBox jComboBoxRefModelAnlise;
    private javax.swing.JComboBox jComboBoxRefModelExp;
    private javax.swing.JComboBox jComboBoxRefModelxClima;
    private javax.swing.JComboBox jComboBoxRefModelxPrecip;
    private javax.swing.JComboBox jComboBoxRegiao;
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
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel10;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JPanel jPanel12;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JPanel jPanel9;
    private javax.swing.JProgressBar jProgressBar1;
    private javax.swing.JRadioButton jRadioButtonUseClima;
    private javax.swing.JRadioButton jRadioButtonUsePrecip;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JTable tabelaEXP;
    private javax.swing.JTextField txtAnaliseTempo;
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
    private javax.swing.JTextField txtPrecipAcumuloExp;
    private javax.swing.JTextField txtPrecipAcumuloObs;
    private javax.swing.JTextField txtPrecipLimite;
    private javax.swing.JTextField txtPrecipMinimo;
    private javax.swing.JTextField txtPrecipRange;
    private javax.swing.JTextField txtPrecipTipo;
    private javax.swing.JTextField txtPrevisaoTempo;
    private javax.swing.JTextField txtPrevisaoTempoTotal;
    private javax.swing.JTextField txtQauntExp;
    private javax.swing.JTextField txtResolucaoX;
    private javax.swing.JTextField txtResolucaoY;
    private javax.swing.JTextArea txtResult;
    // End of variables declaration//GEN-END:variables
}
