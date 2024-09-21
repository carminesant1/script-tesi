# Funzione per imputare la media (per numerici) e la moda (per fattori)
itc <- function(df) {
  mode <- function(x) {  # Il parametro della funzione (x) è la riga su cui calcolare la moda
    x <- na.omit(x)       # Omettiamo i valori mancanti
    if (length(x) == 0) { # Se il vettore è vuoto dopo aver omesso gli NA, 
      stop("Totally incomplete variable")}         # restituisce NA
    tmp <- table(x)                                 # Creiamo una tabella delle frequenze
    uniqv <- names(tmp[which.max(tmp)]) # Troviamo il valore più frequente
    return(uniqv)  # La funzione restituisce la categoria modale
  }
  for (col_name in names(df)) {
    if (is.numeric(df[[col_name]])) {
      # Imputa la media per le variabili numeriche
      mean_value <- mean(df[[col_name]], na.rm = TRUE)
      df[[col_name]][is.na(df[[col_name]])] <- mean_value
    } else if (is.factor(df[[col_name]])) {
      # Imputa la moda per le variabili categoriche o character
      mode_value <- mode(df[[col_name]])
      df[[col_name]][is.na(df[[col_name]])] <- mode_value
    }
  }
  return(df)
}


