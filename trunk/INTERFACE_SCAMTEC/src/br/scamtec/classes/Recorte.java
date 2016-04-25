/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package br.scamtec.classes;

/**
 *
 * @author Rafael Augusto
 */
public class Recorte {
    
    private String analiseTempo;
    private String previsaoTempo;
    private String previsaoTempoTotal;
    private String historiaTempo;
    private String dataInicial;
    private String dataFinal;
    private String horaInicial;
    private String horaFinal;
    private String latEsq;
    private String latDireta;
    private String longInf;
    private String longSup;
    private String resolucaoX;
    private String resolucaoY;
    private String regiao;
    
    public Recorte (
            String AnaliseTempo,
            String PrevisaoTempo,
            String PrevisaoTempoTotal,
            String HistoriaTempo,
            String DataInicial,
            String DataFinal,
            String HoraInicial,
            String HoraFinal,
            String LatEsq,
            String LatDireta,
            String LongInf,
            String LongSup,
            String ResolucaoX,
            String ResolucaoY,
            String Regiao)
    {
        this.analiseTempo = AnaliseTempo;
        this.previsaoTempo= PrevisaoTempo;
        this.previsaoTempoTotal = PrevisaoTempoTotal;
        this.historiaTempo = HistoriaTempo;
        this.dataInicial = DataInicial;
        this.dataFinal = DataFinal;
        this.horaInicial = HoraInicial;
        this.horaFinal = HoraFinal;
        this.latEsq = LatEsq;
        this.latDireta = LatDireta;
        this.longInf = LongInf;
        this.longSup = LongSup;
        this.resolucaoX = ResolucaoX;
        this.resolucaoY = ResolucaoY;
        this.regiao = Regiao;
                
    }
    
    public static String ReplaceEspacoPorAnderline(String palavra){
       String palavraSemEspaco = palavra.replaceAll(" ", "_"); 
       
       return palavraSemEspaco;
    }

    /**
     * @return the analiseTempo
     */
    public String getAnaliseTempo() {
        return analiseTempo;
    }

    /**
     * @param analiseTempo the analiseTempo to set
     */
    public void setAnaliseTempo(String analiseTempo) {
        this.analiseTempo = analiseTempo;
    }

    /**
     * @return the previsaoTempo
     */
    public String getPrevisaoTempo() {
        return previsaoTempo;
    }

    /**
     * @param previsaoTempo the previsaoTempo to set
     */
    public void setPrevisaoTempo(String previsaoTempo) {
        this.previsaoTempo = previsaoTempo;
    }

    /**
     * @return the previsaoTempoTotal
     */
    public String getPrevisaoTempoTotal() {
        return previsaoTempoTotal;
    }

    /**
     * @param previsaoTempoTotal the previsaoTempoTotal to set
     */
    public void setPrevisaoTempoTotal(String previsaoTempoTotal) {
        this.previsaoTempoTotal = previsaoTempoTotal;
    }

    /**
     * @return the historiaTempo
     */
    public String getHistoriaTempo() {
        return historiaTempo;
    }

    /**
     * @param historiaTempo the historiaTempo to set
     */
    public void setHistoriaTempo(String historiaTempo) {
        this.historiaTempo = historiaTempo;
    }

    /**
     * @return the dataInicial
     */
    public String getDataInicial() {
        return dataInicial;
    }

    /**
     * @param dataInicial the dataInicial to set
     */
    public void setDataInicial(String dataInicial) {
        this.dataInicial = dataInicial;
    }

    /**
     * @return the dataFinal
     */
    public String getDataFinal() {
        return dataFinal;
    }

    /**
     * @param dataFinal the dataFinal to set
     */
    public void setDataFinal(String dataFinal) {
        this.dataFinal = dataFinal;
    }

    /**
     * @return the latEsq
     */
    public String getLatEsq() {
        return latEsq;
    }

    /**
     * @param latEsq the latEsq to set
     */
    public void setLatEsq(String latEsq) {
        this.latEsq = latEsq;
    }

    /**
     * @return the latDireta
     */
    public String getLatDireta() {
        return latDireta;
    }

    /**
     * @param latDireta the latDireta to set
     */
    public void setLatDireta(String latDireta) {
        this.latDireta = latDireta;
    }

    /**
     * @return the longInf
     */
    public String getLongInf() {
        return longInf;
    }

    /**
     * @param longInf the longInf to set
     */
    public void setLongInf(String longInf) {
        this.longInf = longInf;
    }

    /**
     * @return the longSup
     */
    public String getLongSup() {
        return longSup;
    }

    /**
     * @param longSup the longSup to set
     */
    public void setLongSup(String longSup) {
        this.longSup = longSup;
    }

    /**
     * @return the resolucaoX
     */
    public String getResolucaoX() {
        return resolucaoX;
    }

    /**
     * @param resolucaoX the resolucaoX to set
     */
    public void setResolucaoX(String resolucaoX) {
        this.resolucaoX = resolucaoX;
    }

    /**
     * @return the resolucaoY
     */
    public String getResolucaoY() {
        return resolucaoY;
    }

    /**
     * @param resolucaoY the resolucaoY to set
     */
    public void setResolucaoY(String resolucaoY) {
        this.resolucaoY = resolucaoY;
    }

    /**
     * @return the regiao
     */
    public String getRegiao() {
        return regiao;
    }

    /**
     * @param regiao the regiao to set
     */
    public void setRegiao(String regiao) {
        this.regiao = regiao;
    }

    /**
     * @return the horaInicial
     */
    public String getHoraInicial() {
        return horaInicial;
    }

    /**
     * @param horaInicial the horaInicial to set
     */
    public void setHoraInicial(String horaInicial) {
        this.horaInicial = horaInicial;
    }

    /**
     * @return the horaFinal
     */
    public String getHoraFinal() {
        return horaFinal;
    }

    /**
     * @param horaFinal the horaFinal to set
     */
    public void setHoraFinal(String horaFinal) {
        this.horaFinal = horaFinal;
    }
}
