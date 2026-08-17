lookup <- c(
  AUG = "Methionine",
  UUU = "Phenylalanine", UUC = "Phenylalanine",
  UUA = "Leucine", UUG = "Leucine",
  UCU = "Serine", UCC = "Serine", UCA = "Serine", UCG = "Serine",
  UAU = "Tyrosine", UAC = "Tyrosine",
  UGU = "Cysteine", UGC = "Cysteine",
  UGG = "Tryptophan",
  UAA = "STOP", UAG = "STOP", UGA = "STOP"
)

translate <- function(bases) {
  if (nchar(bases) == 0) return(c())
  
  codons <- str_extract_all(bases, ".{3}")[[1]]
  
  # 1. Check for STOP codon
  stop_pos <- match(TRUE, codons %in% c("UAA", "UAG", "UGA"))
  
  # 2. Check if string has incomplete codons BEFORE a STOP codon
  if (is.na(stop_pos)) {
    if (nchar(bases) %% 3 != 0) stop("Invalid sequence length")
  } else {
    # If STOP exists, keep only codons before it
    if (stop_pos == 1) return(c())
    codons <- codons[1:(stop_pos - 1)]
  }
  
  # 3. Check for unknown codons
  if (any(!codons %in% names(lookup))) {
    stop("Invalid codon found")
  }
  
  unname(lookup[codons])
}
