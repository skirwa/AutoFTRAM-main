
# DB - Database
# k - Number of  rules that the user wants to be generated.
# L - A set containing valid rules. If the support of a rule meets the minimum support value,
#     then the rule is added to the list L.
#     L contains the k most frequent association rules that meet the minimum support and confidence values.
# I - A set of items (i, j). 


# Algorithm 1 - ftarm algorithm

ftarm <- function(DB, k, minconf) {
  R <- list()
  L <- list()
  # Scan transaction list from the database to get the itemSet I 
  I <- list()
  
  Initialize_Remove(DB, k, mconf)
  
  # Select two items
  for (i in DB) {
    for (j in DB) {
     if (i != j & sup(i) >= minsup & sup(j) >= minsup) {
       newItems <- i.append(j)
       if (sup(newItems) >= minsup) {
         if (conf(i, j) >= minconf) {
           Save(newSet, L, k, minsup)
         }
         if (conf(j, i) >= minconf) {
           Save(newSet, L, k, minsup)
         }
         # compute MAXITEM here
         # update R
       }
     } 
    }
  }
  for (r in R) {
    if (sup(r) > minsup) {
      for (r in R) {
        if (maxSupR == 0 | sup(r) > maxSupR) {
          maxSupR <- r
        } 
      }
      if (maxSupR.expandLR == TRUE) {
        Expand_L(r, L, R, k, minsup, minconf)
        Expand_R(r, L, R, k, minsup, minconf)
      } else {
        Expand_R(r, L, R, k, minsup, minconf)
      }
      
      newR <- list()
      for (r1 in R) {
        if (r != r1 & sup(r1) >= minsup) {
          newR <- newR.append(r1)
        }
      }
    }
    R <- newR
  }
  return (L)
}


# Algorithm 2 - Initialize_Remove

Initialize_Remove <- function (DB, k, mconf) {
  # Calculate minimum number of items m required to generate k rules based on k.
  m <- k * (2/15) # Derived from the example in the article (Propert 5). 
  
  # Sort items in the database based descending order of support.
  DB1 <- sortDescBasedOnSup(DB) 
  
  # A list of minimum number of items m required to generate k rules.
  I <- head(DB1,m) 

  # For each item in the list I:
  for (i in I) {   
    if (minConf(I) >= mconf) { 
      break;
    }
  }
  msup <- minSup(DB, I)
  
  # For each item in list I:
  for (i in I) { 
    # If it's support value is less than the minimum support value:
    if (sup(i) < msup) {
      # Remove the item from the list.  
      I[i] <- NULL 
    }
  }
}

# Calculate the support value for an item.
sup <- function(DB, i) {
  # Support is the no of transactions (I) in DB with item i / total number of transactions in DB.   
  support <- length(tids(i)) / length(DB)
  return(support)
}

# Calculate the confidence value for an item
conf <- function(DB, x, y) {
    totalItems = append(x, y)
    # Confidence is the no of transactions with items x and y / no of transactions with item x.
    confidence = length(tids(totalItems)/length(tids(x)))
    return (confidence)
}

minConf <- function(x, DB) {
  maxTid = -1
  print(x)
  for (i in x) {
    if (length(tid(i)) > maxTid) {
      maxTid = length(tid(i))
    }
  }
  return(length(tids(x))/maxTid)
}

minSup <- function(x, data) {
   return(length(tids(x))/length(data))
}

tids <- function(x, data) {
  # query from db to get the tids for given set of items
}

sortDescBasedOnSup <- function(x) {
  swap_done <- TRUE
  while (swap_done) {
    swap_done <- FALSE
    for (i in 1:(length(x) - 1)) {
      if (sup(x[i]) < sup(x[i + 1])) {
        tmp <- x[i]
        x[i] <- x[i + 1]
        x[i + 1] <- tmp
        swap_done <- TRUE
      }
    }
  }
  return(x)
}

# Algorithm 3 - Save

save <- function(r, L, k, minsup) {
  L <- append(L, r)
  if (length(L) > k) {
    count = 0
    for (rule in L) {
      if (sup(l) == minsup) {
       count <- count + 1
      }
    }
    
    if (length(L) - count >= k) {
      # remove rules with support equal to minsup
      newL <- list()
      for (rule in L) {
        if (sup(l) != minsup) {
          append(newL, l)
        }
      }
      L <- newL
      
      # set minsup to lowest support of rules in L
      newMinSup <- 99999999
      for (rule in L) {
        if (sup(l) < newMinSup) {
          newMinSup <- sup(l)
        }
      }
      minsup <- newMinSup
      
      # Find max item based on minsup and position in total order 
      MaxItem <- 0
      for (i in I) {
        if (sup(i) > minsup & i.totalOrderPosition > MaxItem.totalOrderPosition ) {
          MaxItem <- i
        }
      }
    }
  }
}


# Algorithm 4 - expand_L
expand_L <- function(r, L, R, k, minsup, minconf) {
  # Scan transaction list from db to get the itemSet I
  # FIXME: I should be a candidate item set of the ANTECEDENT of r
  I <- list()
  
  for (i in I) {
    r1.itemSet <- append(r.itemSet, i)
    
    if (sup(r1) >= minsup) {
      if (conf(r1.itemset, i) >= minconf) {
        save(r1, L, k, minsup)
      }
      expandLR <- TRUE
      # FIXME: Only check largest item, not all
      for (i in r1.itemSet) {
        if (i.totalOrderPosition > MaxItem.totalOrderPosition ) {
          expandLR <- FALSE
        } else {
          expandLR <- TRUE
        }
      }
    }
    R <- append(R, r1)
  }
}

# Algorithm 5 - expand_R
  expand_R <- function(r, L, R, k, minsup, minconf) {
    # Scan transaction list from db to get the itemSet I
    # FIXME: I should be a candidate item set of the CONSEQUENT of r
    I <- list()
    for (i in I) { # for every item i in I
      # add i to the consequent of r to get r'
      r1.itemSet <- append(r.itemSet, i)
      # if the support value of r' is greater than or equal to
      # our provided minimum support value
      if (sup(r1) >= minsup) {
        if (conf(r.itemset, i) >= minconf) { # and
        # save with new rule r', but the same L, k, and minimum support value
          save(r1, L, k, minsup)
        }
        # FIXME: if the LARGEST item of the CONSEQUENT of r' is LESS THAN MaxItem
        for (i in r1.itemSet) {
          if (i.totalOrderPosition > MaxItem.totalOrderPosition ) {
            expandLR <- FALSE
            R <- append(R, r1) # FIXME: algorithm: R := R union {r'}
          }
        }
      }
    }
  }
