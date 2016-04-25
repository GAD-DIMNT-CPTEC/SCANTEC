/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package br.scamtec.classes;

import java.awt.Component;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import javax.swing.JOptionPane;

/**
 *
 * @author Rafael ADS Reis
 * @lcal CPTEC INPE - Cachoera Paulista
 * @data Inicio: 03/10/2013 Termino:
 * @dica
 *  Para entender o código passo a passo tire os comentários
 *  dos System.out.prints, e acompanhe os resultados no console.
 * 
 * 
 */
public class GraficoComparativo {
    
    private Component rootPane;
    private static double[][] matrizA = new double[10][21];
    private static double[][] matrizB = new double[10][21];
    private static String[] variaveis = new String[23];
    
    public static String[] LerVariaveis(String variaveis[], String documento) {
        File arquivo = new File(documento);
        //faz a leitura do arquivo
        FileReader fr;
        try {
            fr = new FileReader(arquivo);
            BufferedReader br = new BufferedReader(fr);
            int cont = 0;
            //equanto houver mais linhas
            while (br.ready()) {
                //lê a proxima linha
                String linha = br.readLine();
                if (cont < 1) {
                    String[] stringDividida = linha.split("[\\s]+");
                    for (int i = 0; i < stringDividida.length; i++) {
                        variaveis[i] = stringDividida[i].toString();
                    }
                } else {
                    break;
                }
                cont++;
                
                //System.out.println("\n----------------------------------\n");
                //System.out.println(linha);
            }
            br.close();
            fr.close();
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
        } finally {
            return variaveis;
        }
        
    }
    
    public static double[][] lerResultadosDoScamtec(String documento) throws IOException {
        double[][] matriz = new double[10][21];
        //System.out.println("\n----------------Ler a Matriz");
        File arquivo = new File(documento);
        //faz a leitura do arquivo
        FileReader fr;
        try {
            fr = new FileReader(arquivo);
            BufferedReader br = new BufferedReader(fr);
            int cont = 0;
            //equanto houver mais linhas
            while (br.ready()) {
                //lê a proxima linha
                String linha = br.readLine();
                if (cont > 1) {
                    String[] stringDividida = linha.split("[\\s]+");
                    
                    for (int i = 1; i < stringDividida.length - 2; i++) {
                        matriz[cont - 2][i - 1] = Double.valueOf(stringDividida[i]);
                        /*System.out.print(stringDividida[i] + ": "+ i +" ");*/
                        //System.out.print(matriz[cont - 2][i - 1] + " - ");
                        /*System.out.print(i-1+" "+cont+ " "+(cont-2));*/
                    }
                       //System.out.println("");
                }
                cont++;
                //System.out.println("\n----------------------------------\n");
                //System.out.println(linha);
            }
            br.close();
            fr.close();
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
        } finally {
            return matriz;
        }
    }
    
    public static double[][] TirarDiferencaDaMatrizAeB(double matrizA[][], double matrizB[][]) {
        double[][] matriz = new double[10][21];
        //System.out.println("\n----------------Diferença das Matrizes");
        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 21; j++) {
                if (j > 0) {
                    matriz[i][j] = matrizA[i][j] - matrizB[i][j];
                } else {
                    matriz[i][j] = matrizA[i][j];
                }
                
                //System.out.print(matriz[i][j] + "    |    ");
            }
                //System.out.println("");
        }
        return matriz;
    }
    
    public static double[][] TirarDiferencaDaMatrizAeBVIES(double matrizA[][], double matrizB[][]) {
        double[][] matriz = new double[10][21];
        //System.out.println("\n----------------Diferença das Matrizes");
        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 21; j++) {
                if (j > 0) {
                    matriz[i][j] = Math.abs(matrizA[i][j]) - Math.abs(matrizB[i][j]);
                } else {
                    matriz[i][j] = Math.abs(matrizA[i][j]);
                }
                 //System.out.print(matriz[i][j] + "    |    ");
            }
                 //System.out.println("");
        }
        return matriz;
    }
    
    public static double[][] dividirMatrizEmDois(double matrizAB[][]) {
        double[][] matriz = new double[5][21];
        int contador = 0;
        //System.out.println("\n-----------------Divisão das matrizes");
        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 21; j++) {
                matriz[contador][j] = (matrizAB[i][j] + matrizAB[i + 1][j]) / 2;//uma linha mais a próxima
                //System.out.print(matriz[contador][j] + " = " + "("+matrizAB[i][j]+" + "+matrizAB[i+1][j]+") / 2) = ");
                //System.out.print(matriz[contador][j] + " |\n\n ");
            }
                //System.out.println("");
            i++;
            contador++;
        }
        return matriz;
    }
    
    public static int[][] VerificarImpacto(double matrizAB[][]) {
        int[][] matriz = new int[5][20];
        for (int i = 0; i < 5; i++) {
            for (int j = 1; j < 21; j++) {
                if (matrizAB[i][j] >= 0.10) {
                    matriz[i][j - 1] = 5;
                } else if (matrizAB[i][j] >= 0.03) {
                    matriz[i][j - 1] = 4;
                } else if (matrizAB[i][j] >= -0.03) {
                    matriz[i][j - 1] = 3;
                } else if (matrizAB[i][j] < -0.03) {
                    matriz[i][j - 1] = 2;
                } else if (matrizAB[i][j] <= -0.10) {
                    matriz[i][j - 1] = 1;
                }
                //System.out.print(matriz[i][j - 1] + " | ");
            }
                //System.out.println("");
        }
        return matriz;
    }
    
    public static int[][] transporMatriz(int matriz[][]) {
        int[][] matrizTransposta = new int[20][5];
        for (int i = 0; i < 20; i++) {
            for (int j = 0; j < 5; j++) {
                matrizTransposta[i][j] = matriz[j][i];
                //System.out.print(matrizTransposta[i][j] + " | ");
            }
                //System.out.println("");
        }
        return matrizTransposta;
    }
    
    public static double[][] MultiplicarMatrizPorMenosUm(double[][] matriz) {
        double[][] matrizMultiplicada = new double[10][21];
        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 21; j++) {
                if (j > 0) {
                    matrizMultiplicada[i][j] = (matriz[i][j]) * (-1);
                } else {
                    matrizMultiplicada[i][j] = (matriz[i][j]);
                }
                 //System.out.print(matrizMultiplicada[i][j] + "  |  ");
            }
                //System.out.println("");
            
        }
        return matrizMultiplicada;
    }
    
    public static double[] CalcularDesvioPadrao(double[][] matriz) {
        double[] desvioPadrao = new double[20];
        //System.out.println("Desvio Padrão \n\n\n");
        double media = 0;
        for (int i = 1; i < 21; i++) {
            for (int j = 0; j < 10; j++) {
                if (i > 0) {
                    media = media + matriz[j][i];
                    if (j == 9) {
                        desvioPadrao[i - 1] = media / 11;
                    }
                }
                
            }
            if (i > 0) {
               // System.out.print(desvioPadrao[i - 1] + " | ");
            }
            
            media = 0;
        }
        media = 0;
        for (int i = 0; i < 20; i++) {
            for (int j = 0; j < 10; j++) {
                if (i > 0) {
                    media = media + Math.pow((matriz[j][i] - desvioPadrao[i - 1]), 2);
                    if (j == 9) {
                        desvioPadrao[i - 1] = media + Math.pow((0 - desvioPadrao[i - 1]), 2);
                        desvioPadrao[i - 1] = Math.sqrt(desvioPadrao[i - 1] / 10);
                    }
                }
            }
            //System.out.print(desvioPadrao[i] + " | ");
            media = 0;
        }
        
        
        return desvioPadrao;
    }
    
    public static int[][] VerificarImpactoRMS(double matrizAB[][], double desvioPadrao[]) {
        int[][] matriz = new int[5][20];
        
        for (int i = 0; i < 5; i++) {
            for (int j = 1; j < 21; j++) {
                if (matrizAB[i][j] >= desvioPadrao[j - 1]) {
                    matriz[i][j - 1] = 5;
                } else if ((matrizAB[i][j] >= (desvioPadrao[j - 1] * 0.1)) && (matrizAB[i][j] < desvioPadrao[j - 1])) {
                    matriz[i][j - 1] = 4;
                } else if ((matrizAB[i][j] >= ((desvioPadrao[j - 1] * -1) * 0.1)) && (matrizAB[i][j] < (desvioPadrao[j - 1] * 0.1))) {
                    matriz[i][j - 1] = 3;
                } else if ((matrizAB[i][j] < ((desvioPadrao[j - 1] * -1) * 0.1)) && (matrizAB[i][j] > (desvioPadrao[j - 1] * -1))) {
                    matriz[i][j - 1] = 2;
                } else if (matrizAB[i][j] <= desvioPadrao[j - 1] * -1) {
                    matriz[i][j - 1] = 1;
                }
                //System.out.print(matriz[i][j - 1] + " | ");
            }
                //System.out.println("");
        }
        return matriz;
    }
    
    public static int[][] VerificarImpactoVIES(double matrizAB[][], double desvioPadrao[]) {
        int[][] matriz = new int[5][20];
        
        for (int i = 0; i < 5; i++) {
            for (int j = 1; j < 21; j++) {
                if (matrizAB[i][j] >= desvioPadrao[j - 1]) {
                    matriz[i][j - 1] = 5;
                } else if ((matrizAB[i][j] >= (desvioPadrao[j - 1] * 0.1)) && (matrizAB[i][j] < desvioPadrao[j - 1])) {
                    matriz[i][j - 1] = 4;
                } else if ((matrizAB[i][j] >= ((desvioPadrao[j - 1] * -1) * 0.1)) && (matrizAB[i][j] < (desvioPadrao[j - 1] * 0.1))) {
                    matriz[i][j - 1] = 3;
                } else if ((matrizAB[i][j] < ((desvioPadrao[j - 1] * -1) * 0.1)) && (matrizAB[i][j] > (desvioPadrao[j - 1] * -1))) {
                    matriz[i][j - 1] = 2;
                } else if (matrizAB[i][j] <= desvioPadrao[j - 1] * -1) {
                    matriz[i][j - 1] = 1;
                }
               //System.out.print(matriz[i][j - 1] + " | ");
            }
               //System.out.println("");
        }
        return matriz;
    }
    
    public static int[][] GerarMatrizParaContruirGrafico(String arquivo1, String arquivo2) throws IOException {
        double[][] matriz = new double[10][21];
        double[][] matrizAB = new double[5][21];
        int[][] matrizImpacto = new int[5][21];
        matrizA = GraficoComparativo.lerResultadosDoScamtec(arquivo1);
        matrizB = GraficoComparativo.lerResultadosDoScamtec(arquivo2);

        /*
        for (int i = 1; i < 21; i++) {
        System.out.print(matrizA[1][i] + " ");
        }
        System.out.println();
        for (int i = 1; i < 21; i++) {
        System.out.print(matrizB[1][i] + " ");
        }
         */
        matriz = GraficoComparativo.TirarDiferencaDaMatrizAeB(matrizA, matrizB);
        /*System.out.println("\n\n");*/
        matrizAB = GraficoComparativo.dividirMatrizEmDois(matriz);
        //System.out.println("\n");
        matrizImpacto = GraficoComparativo.VerificarImpacto(matrizAB);
        //System.out.println("\n");
        
        return GraficoComparativo.transporMatriz(matrizImpacto);
        
    }
    
    public static int[][] GerarMatrizParaContruirGraficoRMS(String arquivo1, String arquivo2) throws IOException {
        matrizA = GraficoComparativo.lerResultadosDoScamtec(arquivo1);
        matrizB = GraficoComparativo.lerResultadosDoScamtec(arquivo2);
        double[][] matriz = new double[10][21];
        double[] desvioPadrao = new double[20];
        double[][] matrizAB = new double[5][21];
        int[][] matrizImpacto = new int[5][21];
        matriz = GraficoComparativo.TirarDiferencaDaMatrizAeB(matrizA, matrizB);
        matriz = GraficoComparativo.MultiplicarMatrizPorMenosUm(matriz);
        desvioPadrao = GraficoComparativo.CalcularDesvioPadrao(matriz);//Voltar para o número 20 do desvio padrão
        matrizAB = GraficoComparativo.dividirMatrizEmDois(matriz);
        matrizImpacto = GraficoComparativo.VerificarImpactoRMS(matrizAB, desvioPadrao);
        
                
        return GraficoComparativo.transporMatriz(matrizImpacto);
    }
    
    public static int[][] GerarMatrizParaContruirGraficoVIES(String arquivo1, String arquivo2) throws IOException {
        matrizA = GraficoComparativo.lerResultadosDoScamtec(arquivo1);
        matrizB = GraficoComparativo.lerResultadosDoScamtec(arquivo2);
        double[][] matriz = new double[10][21];
        double[] desvioPadrao = new double[20];
        double[][] matrizAB = new double[5][21];
        int[][] matrizImpacto = new int[5][21];
        matriz = GraficoComparativo.TirarDiferencaDaMatrizAeBVIES(matrizA, matrizB);
        matriz = GraficoComparativo.MultiplicarMatrizPorMenosUm(matriz);
        desvioPadrao = GraficoComparativo.CalcularDesvioPadrao(matriz);//Voltar para o número 20 do desvio padrão
        matrizAB = GraficoComparativo.dividirMatrizEmDois(matriz);
        matrizImpacto = GraficoComparativo.VerificarImpactoVIES(matrizAB, desvioPadrao);
        
        
        
        return GraficoComparativo.transporMatriz(matrizImpacto);
    }
    /*public void copiarPastaDeImagens(File origem, File destino, boolean overwrite) throws IOException, UnsupportedOperationException
    {
    if (!destino.exists()) {  
    destino.mkdir();  
    }  
    if (!origem.isDirectory()) {  
    throw new UnsupportedOperationException("Origem deve ser um diretório");  
    }  
    if (!destino.isDirectory()) {  
    throw new UnsupportedOperationException("Destino deve ser um diretório");  
    }  
    File[] files = origem.listFiles();  
    for (int i = 0; i < files.length; ++i) {  
    if (files[i].isDirectory()) {  
    copiarPastaDeImagens(files[i], new File(destino + "/" + files[i].getName()), overwrite);  
    } else {  
    System.out.println("Copiando arquivo: " + files[i].getName());  
    copiarPastaDeImagens(files[i], new File(destino + "/" + files[i].getName()), overwrite);  
    }  
    }  
    }*/
    
    public void GerarHtmlComGraficoComparativo(String ACOR1, String ACOR2, String RMS1, String RMS2, String VIES1, String VIES2, String saida, String recorte) throws IOException {
        int[][] matrizTransposta = new int[20][5];
        
        String saidaFinal = saida + Recorte.ReplaceEspacoPorAnderline(recorte);
        
        variaveis = GraficoComparativo.LerVariaveis(variaveis, ACOR1);
        matrizTransposta = GraficoComparativo.GerarMatrizParaContruirGrafico(ACOR1, ACOR2);
        
        //GraficoComparativo.copiarPastaDeImagens(new File(""),new File(saida));
        for (int i = 19; i >= 0; i--) {
            for (int j = 4; j >= 0; j--) {
                //System.out.print(matrizTransposta[i][j] + " | ");
            }
               //System.out.println();
        }
        String BarraDoSistema;
        if (System.getProperty("os.name").contains("Linux")) {
            BarraDoSistema = "/";
        } else {
            BarraDoSistema = "\\";
        }
        
                
        copyDirectory(new File("images" + BarraDoSistema), new File(saidaFinal +  BarraDoSistema + "images" + BarraDoSistema));
        File arquivo = new File(saidaFinal +  BarraDoSistema + "grafico_impacto.html");
        try {
            
            if (!arquivo.exists()) {
                arquivo.createNewFile();
            } else {
                arquivo.delete();
            }
            FileWriter fw = new FileWriter(arquivo, true);
            
            BufferedWriter bw = new BufferedWriter(fw);
            
            String html =
                    "<!DOCTYPE html>"
                    + "\n<html lang=\"pt-BR\">"
                    + "\n<head>"
                    + "\n<meta charset=\"UTF-8\" />"
                    + "\n<style Type=\"text/css\">"
                    //+ "\n#Titulo {position:absolute; margin-left:600px}"
                    + "\n#RetanguloInvisivelDireita {margin-top:20px; width:100px; height:740px;  float:left}"
                    + "\n.QuadradoInvisivelNoRetanguloInvisivelDireita {width:100px;height:39px; float:right;}"
                    + "\n.PalavraNoQuadradoInvisivel {margin-top:12px;margin-right:5px;float:right;font-size:12px;}"
                    + "\n#PrimeiraFraseAcimaDoQuadrado {Position: absolute;margin-top:0px;margin-left:200px;}"
                    + "\n#PrimeiroQuadradoPai {margin-top:20px; width:350px; height:778px; border: 1px solid #000000; float:left}"
                    + "\n.QuadradoFilho {width:68px;height:37px; border: 1px solid #000000;float:left;}"
                    + "\n#FraseAcimaDoQuadradoPai2 {Position: absolute;margin-top:0px;margin-left:650px;}"
                    + "\n.QuadradoPai2e3 {margin-top:20px;margin-left:50px; width:350px; height:778px; border: 1px solid #000000;float:left}"
                    + "\n#FraseAcimaDoQuadradoPai3 {Position: absolute;margin-top:0px;margin-left:1050px;}"
                    + "\nimg { margin-left:15px;margin-top:5px;  }"
                    + "\nimg Little{margin-left:20px;}"
                    + "\n</style>"
                    + "\n<title>Domain: " + recorte + "</title>"
                    + "\n</head> "
                    + "\n<body >"
                    + "\n<!--<div style=\"position:absolute; top:10%; left:12%; right:12%; width:76%; height:89%; border: 1px solid #000000; \"></div>-->"
                    //+ "\n<legend id=\"Titulo\">Domain: South Hemisphere</legend>"
                    + "\n<div id=\"RetanguloInvisivelDireita\" >";
            for (int i = 19; i >= 0; i--) {
                html += "\n<div class=\"QuadradoInvisivelNoRetanguloInvisivelDireita\"><span class=\"PalavraNoQuadradoInvisivel\" >" + variaveis[i + 1] + "</span></div>";
            }
            
            html += "\n</div>"
                    + "\n<legend id=\"PrimeiraFraseAcimaDoQuadrado\" >Anomaly Correlation</legend>"
                    + "\n<div id=\"PrimeiroQuadradoPai\" >";
            for (int i = 19; i >= 0; i--) {
                html += "<a href=\"" + saidaFinal + BarraDoSistema + "images" + BarraDoSistema + "ACOR_" + variaveis[i + 1] + "_" + Recorte.ReplaceEspacoPorAnderline(recorte) + ".png\">";
                for (int j = 0; j < 5; j++) {
                    switch (matrizTransposta[i][j]) {
                        case 5:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 5 + ".png\" alt=\"\" /></div>";
                            break;
                        case 4:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 4 + ".png\" alt=\"\" /></div>";
                            break;
                        case 3:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 3 + ".png\" alt=\"\" /></div>";
                            break;
                        case 2:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 2 + ".png\" alt=\"\" /></div>";
                            break;
                        case 1:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 1 + ".png\" alt=\"\" /></div>";
                            break;
                        default:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 0 + ".png\" alt=\"\" /></div>";
                            break;
                    }
                    
                }
                html += "</a>";
                //System.out.println("");
            }
            html +=
                    "\n</div>"
                    + "\n<legend id=\"FraseAcimaDoQuadradoPai2\">RMS Error</legend>"
                    + "\n<div class=\"QuadradoPai2e3\">";
            matrizTransposta = GraficoComparativo.GerarMatrizParaContruirGraficoRMS(RMS1, RMS2);
            for (int i = 19; i >= 0; i--) {
                html += "<a href=\"" + saidaFinal + BarraDoSistema + "images" + BarraDoSistema + "RMSE_" + variaveis[i + 1] + "_" + Recorte.ReplaceEspacoPorAnderline(recorte) + ".png\">";
                for (int j = 0; j < 5; j++) {
                    switch (matrizTransposta[i][j]) {
                        case 5:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 5 + ".png\" alt=\"\" /></div>";
                            break;
                        case 4:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 4 + ".png\" alt=\"\" /></div>";
                            break;
                        case 3:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 3 + ".png\" alt=\"\" /></div>";
                            break;
                        case 2:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 2 + ".png\" alt=\"\" /></div>";
                            break;
                        case 1:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 1 + ".png\" alt=\"\" /></div>";
                            break;
                        default:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 0 + ".png\" alt=\"\" /></div>";
                            break;
                    }
                }
                html += "</a>";
                //System.out.println("");
            }
            
            html +=
                    "\n</div>"
                    + "\n <legend id=\"FraseAcimaDoQuadradoPai3\">Vies Error</legend>"
                    + "\n<div class=\"QuadradoPai2e3\">";
            matrizTransposta = GraficoComparativo.GerarMatrizParaContruirGraficoVIES(VIES1, VIES2);
            for (int i = 19; i >= 0; i--) {
                html += "<a href=\"" + saidaFinal + BarraDoSistema + "images" + BarraDoSistema + "VIES_" + variaveis[i + 1] + "_" + Recorte.ReplaceEspacoPorAnderline(recorte) + ".png\">";
                for (int j = 0; j < 5; j++) {
                    switch (matrizTransposta[i][j]) {
                        case 5:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 5 + ".png\" alt=\"\" /></div>";
                            break;
                        case 4:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 4 + ".png\" alt=\"\" /></div>";
                            break;
                        case 3:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 3 + ".png\" alt=\"\" /></div>";
                            break;
                        case 2:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 2 + ".png\" alt=\"\" /></div>";
                            break;
                        case 1:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 1 + ".png\" alt=\"\" /></div>";
                            break;
                        default:
                            html +=
                                    "\n<div class=\"QuadradoFilho\" ><img src=\"." + BarraDoSistema + "images" + BarraDoSistema + "" + 0 + ".png\" alt=\"\" /></div>";
                            break;
                    }
                }
                html += "</a>";
                //System.out.println("");
            }
            html +=
                    "\n</div>"
                    + "\n</body>"
                    + "\n</html>";
            bw.write(html);
            bw.newLine();
            
            bw.close();
            fw.close();
            
            JOptionPane.showMessageDialog(rootPane, "Gráfico criado com sucesso");
            
            
            try {
                //Process p = Runtime.getRuntime().exec("google-chrome /home/rafael/Documentos/interf_scamtec/Interface_Scamtec_v01/dist/results/grafico.html");
                Process p = Runtime.getRuntime().exec("firefox " + saidaFinal + BarraDoSistema + "grafico_impacto.html");
            } catch (Exception e) {
                JOptionPane.showMessageDialog(null, "Não foi possível abrir o navegador, clique com o botão direito no arquivo grafico.html que está na pasta de resultados e clique em abrir com qualquer navegador.");
                System.err.println(e.toString());  
            }
            
        } catch (IOException ex) {
            JOptionPane.showMessageDialog(rootPane, "ocorreu um merro: " + ex);
        }
    }
    // Copia todos os arquivos de um diretório para o diretório destino
    // Se o diretório destino não existir, ele sera criado automaticamente

    public void copyDirectory(File srcDir, File dstDir) throws IOException {
        if (srcDir.isDirectory()) {
            if (!dstDir.exists()) {
                dstDir.mkdir();
            }            
            String[] children = srcDir.list();
            for (int i = 0; i < children.length; i++) {
                copyDirectory(new File(srcDir, children[i]),
                        new File(dstDir, children[i]));
            }
        } else {
            // Este método está implementado na dica – Copiando um arquivo utilizando o Java
            copyFile(srcDir, dstDir);
        }
    }
    // Copia arquivo desejado, para o arquivo de destino
    // Se o arquivo de destino não existir, ele será criado

    void copyFile(File src, File dst) throws IOException {
        InputStream in = new FileInputStream(src);
        OutputStream out = new FileOutputStream(dst);           // Transferindo bytes de entrada para saída
        byte[] buf = new byte[1024];
        int len;
        while ((len = in.read(buf)) > 0) {
            out.write(buf, 0, len);
        }
        in.close();
        out.close();
    }
}
