/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package br.scamtec.frame;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFrame;
import javax.swing.table.DefaultTableModel;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.ApplicationFrame;

/**
 *
 * @author paulo.henrique
 */
public class Graficos extends javax.swing.JInternalFrame {

    String linha = "";
    List<String> umaLinha = new ArrayList<String>();
    String[] cabecalho = null;
    String[] val = null;
    String[][] valores = new String[14][23];
    private XYSeriesCollection dataset;

    /**
     * Creates new form Graficos
     */
    public Graficos() {

        initComponents();

        jRadioButtonAcor.setSelected(true);
        leituraArqSaida();
        setVariaveis();
        tabelaPadrao();
    }

    private void leituraArqSaida() {
        //---------------------------------------------------------------------------------------------------------------
        String NomeArq = "/home2/paulo.henrique/NetBeansProjects/Interface_Scamtec_v01/" + "ACOREXP01_20040305002004030500.scam";
        //main.endSAida

        try {
            BufferedReader in = new BufferedReader(new FileReader(NomeArq));
            while ((linha = in.readLine()) != null) {
                umaLinha.add(linha);

            }

            //   System.out.println(teste.get(0));

            for (int i = 0; i < umaLinha.size(); i++) {

                if (umaLinha.get(i).contains("%")) {
                    cabecalho = umaLinha.get(i).trim().split(" ");
                    //System.out.println(vetor[1]);

                } else {

                    val = umaLinha.get(i).trim().split("    ");
                    for (int j = 0; j < val.length; j++) {
                        valores[i][j] = val[j];
                    }


                }

            }

        } catch (Exception e) {
            System.err.println("Erro na abertura do arquivo " + NomeArq + '\n' + e);
        }
    }

    private void tabelaPadrao() {
        //leituraArqSaida();
        DefaultTableModel modelo = ((DefaultTableModel) tabelaValores.getModel());
        modelo.setRowCount(0);
        modelo.setColumnCount(0);

        //  int totalIntegracao = ScamtecConfiguracao.totalPrev / ScamtecConfiguracao.passoPrev + 1;
        for (int i = 1; i <= 13; i++) {
            modelo.addColumn(valores[i][0]);

        }


    }

    private void setVariaveis() {
        //leituraArqSaida();

        DefaultComboBoxModel modelo = ((DefaultComboBoxModel) jComboBoxVariavel.getModel());
        modelo.removeAllElements();
        for (int i = 1; i < cabecalho.length; i++) {
            modelo.addElement(cabecalho[i]);
        }

    }

    public void PlotTest() throws FileNotFoundException, IOException {

        //leituraArqSaida();
        dataset = new XYSeriesCollection();

        for (int w = 0; w <= 0; w++) {
            XYSeries data = new XYSeries("EXP" + w);
            

                for (int j = 1; j <= 13; j++) {
                    data.add(j, Double.parseDouble(valores[j][jComboBoxVariavel.getSelectedIndex() + 1]) + w); //Point 1  
                }

                dataset.addSeries(data);
                showGraph("ACOR - " + cabecalho[jComboBoxVariavel.getSelectedIndex()+1]);
      

        }
    }

    private void showGraph(String titulo) throws FileNotFoundException, IOException {
        final JFreeChart chart = createChart(dataset, titulo);
        final ChartPanel chartPanel = new ChartPanel(chart);
        chartPanel.setPreferredSize(new java.awt.Dimension(900, 700));
        JFrame frame = new JFrame(titulo);//Nome no topo do Frame
        frame.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        frame.setContentPane(chartPanel);
        frame.pack();
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



        //salvando os arquivos
        OutputStream arquivo = new FileOutputStream("grafico.png");
        ChartUtilities.writeChartAsPNG(arquivo, chart, 800, 700);


        return chart;
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        buttonGroup1 = new javax.swing.ButtonGroup();
        jScrollPane1 = new javax.swing.JScrollPane();
        tabelaValores = new javax.swing.JTable();
        jRadioButtonVies = new javax.swing.JRadioButton();
        jRadioButtonRmse = new javax.swing.JRadioButton();
        jRadioButtonAcor = new javax.swing.JRadioButton();
        jComboBoxVariavel = new javax.swing.JComboBox();
        btGrafico = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();

        setBorder(null);
        setClosable(true);
        setTitle("GRAFICOS ESTATISTICOS");
        setToolTipText("");

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
        jScrollPane1.setViewportView(tabelaValores);

        buttonGroup1.add(jRadioButtonVies);
        jRadioButtonVies.setText("VIES");

        buttonGroup1.add(jRadioButtonRmse);
        jRadioButtonRmse.setText("RMSE");

        buttonGroup1.add(jRadioButtonAcor);
        jRadioButtonAcor.setText("ACOR");

        jComboBoxVariavel.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        jComboBoxVariavel.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jComboBoxVariavelMouseClicked(evt);
            }
        });
        jComboBoxVariavel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxVariavelActionPerformed(evt);
            }
        });

        btGrafico.setIcon(new javax.swing.ImageIcon(getClass().getResource("/br/scamtec/imagens/line-chart.png"))); // NOI18N
        btGrafico.setText("Grafico");
        btGrafico.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btGraficoActionPerformed(evt);
            }
        });

        jButton1.setText("jButton1");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 795, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jRadioButtonVies)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jRadioButtonRmse)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jRadioButtonAcor)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jComboBoxVariavel, javax.swing.GroupLayout.PREFERRED_SIZE, 114, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButton1)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(btGrafico)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jRadioButtonVies)
                    .addComponent(jRadioButtonRmse)
                    .addComponent(jRadioButtonAcor)
                    .addComponent(jComboBoxVariavel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButton1))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 358, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btGrafico)
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void btGraficoActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btGraficoActionPerformed
        //leituraArqSaida();
        if (jRadioButtonAcor.isSelected() == true) {
            try {
                PlotTest();
            } catch (FileNotFoundException ex) {
                Logger.getLogger(Graficos.class.getName()).log(Level.SEVERE, null, ex);
            } catch (IOException ex) {
                Logger.getLogger(Graficos.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }//GEN-LAST:event_btGraficoActionPerformed

    private void jComboBoxVariavelMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jComboBoxVariavelMouseClicked
    }//GEN-LAST:event_jComboBoxVariavelMouseClicked

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        
    }//GEN-LAST:event_jButton1ActionPerformed

    private void jComboBoxVariavelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxVariavelActionPerformed
        DefaultTableModel modelo = ((DefaultTableModel) tabelaValores.getModel());
        //System.out.println("TESTE " + jComboBoxVariavel.getSelectedIndex());

        modelo.setNumRows(0);
        tabelaValores.updateUI();

        String[] dados = new String[13];
        for (int j = 1; j <= 13; j++) {
            dados[j - 1] = valores[j][jComboBoxVariavel.getSelectedIndex() + 1];

        }
        modelo.addRow(dados);



    }//GEN-LAST:event_jComboBoxVariavelActionPerformed
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btGrafico;
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton jButton1;
    private javax.swing.JComboBox jComboBoxVariavel;
    private javax.swing.JRadioButton jRadioButtonAcor;
    private javax.swing.JRadioButton jRadioButtonRmse;
    private javax.swing.JRadioButton jRadioButtonVies;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable tabelaValores;
    // End of variables declaration//GEN-END:variables
}
