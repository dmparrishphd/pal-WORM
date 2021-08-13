sat <- ( function () {

    .sat <- function ( h , l , precision = 1 , cmax = 180 ) {
        C <- seq ( from = 0 , to = cmax , by = precision )
        COL <- hcl ( h = h , c = C , l = l , fixup = FALSE )
        i <- match ( TRUE , rev ( ! is.na ( COL ) ) )
        rev ( COL ) [[ i ]] }

    function ( h = 0 , l = 85 , precision = 1 ) {
        HL <- cbind ( h , l )
        vapply (
            FUN.VALUE = "" ,
            USE.NAMES = FALSE ,
            X = seq_len ( nrow ( HL ) ) ,
            FUN = function ( i ) .sat (
                h = HL [ i , 1 ] ,
                c = HL [ i , 2 ] ,
                precision = precision ) ) } } ) ()
