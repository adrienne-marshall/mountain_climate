structure(list(name = "biblioNetwork", objs = structure(list(
    `package:bibliometrix` = function (M, analysis = "coupling", 
        network = "authors", sep = ";") 
    {
        crossprod <- Matrix::crossprod
        NetMatrix = NA
        if (analysis == "coupling") {
            switch(network, authors = {
                WA = cocMatrix(M, Field = "AU", type = "sparse", 
                  sep)
                WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                  sep)
                CRA = crossprod(WCR, WA)
                NetMatrix = crossprod(CRA, CRA)
            }, references = {
                WCR = Matrix::t(cocMatrix(M, Field = "CR", type = "sparse", 
                  sep))
                NetMatrix = crossprod(WCR, WCR)
            }, keywords = {
                WK = cocMatrix(M, Field = "ID", type = "sparse", 
                  sep)
                WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                  sep)
                CRK = crossprod(WCR, WK)
                NetMatrix = crossprod(CRK, CRK)
            }, author_keywords = {
                WK = cocMatrix(M, Field = "DE", type = "sparse", 
                  sep)
                WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                  sep)
                CRK = crossprod(WCR, WK)
                NetMatrix = crossprod(CRK, CRK)
            }, sources = {
                WSO = cocMatrix(M, Field = "SO", type = "sparse", 
                  sep)
                WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                  sep)
                CRSO = crossprod(WCR, WSO)
                NetMatrix = crossprod(CRSO, CRSO)
            }, countries = {
                WCO = cocMatrix(M, Field = "AU_CO", type = "sparse", 
                  sep)
                WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                  sep)
                CRCO = crossprod(WCR, WCO)
                NetMatrix = crossprod(CRCO, CRCO)
            })
        }
        if (analysis == "co-citation") {
            switch(network, authors = {
                WA = cocMatrix(M, Field = "CR_AU", type = "sparse", 
                  sep)
                NetMatrix = crossprod(WA, WA)
            }, references = {
                WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                  sep)
                NetMatrix = crossprod(WCR, WCR)
            }, sources = {
                WSO = cocMatrix(M, Field = "CR_SO", type = "sparse", 
                  sep)
                NetMatrix = crossprod(WSO, WSO)
            })
        }
        if (analysis == "collaboration") {
            switch(network, authors = {
                WA = cocMatrix(M, Field = "AU", type = "sparse", 
                  sep)
                NetMatrix = crossprod(WA, WA)
            }, countries = {
                WCO = cocMatrix(M, Field = "AU_CO", type = "sparse", 
                  sep)
                NetMatrix = crossprod(WCO, WCO)
            })
        }
        return(NetMatrix)
    }, function (M, analysis = "coupling", network = "authors", 
        sep = ";") 
    {
        crossprod <- Matrix::crossprod
        NetMatrix = NA
        if (analysis == "coupling") {
            switch(network, authors = {
                WA = cocMatrix(M, Field = "AU", type = "sparse", 
                  sep)
                WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                  sep)
                CRA = crossprod(WCR, WA)
                NetMatrix = crossprod(CRA, CRA)
            }, references = {
                WCR = Matrix::t(cocMatrix(M, Field = "CR", type = "sparse", 
                  sep))
                NetMatrix = crossprod(WCR, WCR)
            }, keywords = {
                WK = cocMatrix(M, Field = "ID", type = "sparse", 
                  sep)
                WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                  sep)
                CRK = crossprod(WCR, WK)
                NetMatrix = crossprod(CRK, CRK)
            }, author_keywords = {
                WK = cocMatrix(M, Field = "DE", type = "sparse", 
                  sep)
                WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                  sep)
                CRK = crossprod(WCR, WK)
                NetMatrix = crossprod(CRK, CRK)
            }, sources = {
                WSO = cocMatrix(M, Field = "SO", type = "sparse", 
                  sep)
                WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                  sep)
                CRSO = crossprod(WCR, WSO)
                NetMatrix = crossprod(CRSO, CRSO)
            }, countries = {
                WCO = cocMatrix(M, Field = "AU_CO", type = "sparse", 
                  sep)
                WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                  sep)
                CRCO = crossprod(WCR, WCO)
                NetMatrix = crossprod(CRCO, CRCO)
            })
        }
        if (analysis == "co-citation") {
            switch(network, authors = {
                WA = cocMatrix(M, Field = "CR_AU", type = "sparse", 
                  sep)
                NetMatrix = crossprod(WA, WA)
            }, references = {
                WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                  sep)
                NetMatrix = crossprod(WCR, WCR)
            }, sources = {
                WSO = cocMatrix(M, Field = "CR_SO", type = "sparse", 
                  sep)
                NetMatrix = crossprod(WSO, WSO)
            })
        }
        if (analysis == "collaboration") {
            switch(network, authors = {
                WA = cocMatrix(M, Field = "AU", type = "sparse", 
                  sep)
                NetMatrix = crossprod(WA, WA)
            }, countries = {
                WCO = cocMatrix(M, Field = "AU_CO", type = "sparse", 
                  sep)
                NetMatrix = crossprod(WCO, WCO)
            })
        }
        return(NetMatrix)
    }), .Names = c("package:bibliometrix", "")), where = c("package:bibliometrix", 
"namespace:bibliometrix"), visible = c(TRUE, FALSE), dups = c(FALSE, 
TRUE)), .Names = c("name", "objs", "where", "visible", "dups"
), class = "getAnywhere")
