# Work-Bank-Final
Trabajo universitario




paises <- c("Chile", "México", "Perú", "Colombia", "España")
objetivo <- "Perú"

i <- 1

while (i <= length(paises)) {
  if (paises[i] == objetivo) {
    print(paste("¡País encontrado en la posición!", i))
    break # Detiene la búsqueda en cuanto encuentra "Perú"
  }
  i <- i + 1
}
