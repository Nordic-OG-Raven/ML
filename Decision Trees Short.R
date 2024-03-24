# Simple Decision Trees are great for in particular interpretation by way of visualization.
# Ensemble Decision Trees are on the other hand more useful for predictions.

# Decision Trees ----
    # Adv: simple and transparent 
    # Disadv: unstable

    # Libraries ----
        library(tree)
        library(ISLR)
        attach(Carseats)
        library(MASS)
        library(rattle)      
        library(vip)         
        library(pdp)         
        attach(Boston)
        library(randomForest)
        library(gbm)
        library(Hmisc)
        library(caret)
        library(rpart)
        library(rpart.plot)
        library(ranger)     # a c++ implementation of random forest 
        library(h2o)        # a java-based implementation of random forest
        # NOTE: in ISL, we have use the original randomForest package (Liaw and Wiener 2002).
        # ranger and h2o are the most modern implementations; they allow for parallelization to improve training time.
        # At the exam we will mostly use ranger (because the rest can take a long time to load)

    # rpart visualization ----
      rt.carseats <- rpart(Sales ~ ., data = Carseats.train)
      prp(rt.carseats, extra=101, box.col = "yellow", split.box.col ="grey")
      # Shelve location is the most important variable in determining the sales (number of sold units),
      # and stores with bad and medium shelve location sell less  units than stores with good shelve location.
      # Furthermore, Price and Age play a significant role in determining the sales. 
      # The tree predicts for example that stores with a good shelve location and a price for carseats less than 107.5 will sell on average 12.53 units (the highest). 

    # Regression Trees ----
      RNGkind("L'Ecuyer-CMRG")

          # Raw Tree ----
    
              # Splitting and fitting on training data
                set.seed(1)
                train = sample(1:nrow(Boston), nrow(Boston)/2)
                tree.boston=tree(medv~.,Boston,subset=train)
                summary(tree.boston)
              
              # Plotting 
                par(mfrow=c(1,1))
                plot(tree.boston)
                text(tree.boston,pretty=0)
                # most expensive houses are asssociated with larger homes (rm > 7.45), 
                # for which the tree predicts a median house price of 45.38
                # or smaller houses (rm<5.49) 
                # but for which the weighted mean of distances to five Boston employment centres (dis) is short (dis<3.25); 
                # in this case the tree predicts a median house price of 43.28 
    
          # Pruning ----
                  # Fitting and plotting ----
                    cv.boston = cv.tree(tree.boston)
                    plot(cv.boston$size, cv.boston$dev, type='b')
                    # in this case 9 terminal nodes returns the smallest error; 
                    # this is the most complex model 
                    # meaning that pruning the tree is not useful in this case; 
                    # if pruning is required, as an example, let us choose best = 5
                      prune.boston = prune.tree(tree.boston,best=5)
                      plot(prune.boston)
                      text(prune.boston,pretty=0)
        
                  # Evaluating test error ----
                    
                    # Predicting based on training data
                      yhat = predict(tree.boston,newdata = Boston[-train,])
                      boston.test = Boston[-train,"medv"]
                      
                    # Comparing with test data
                      plot(yhat,boston.test)
                      abline(0,1)
                    
                    # Obtaining test MSE
                      mean((yhat-boston.test)^2) 
                      # [1] 24.59711 ~ 25.000  (=MSE)
                      # meaning that the model leads to test predictions that are within around 
                      # 5000 (sqrt(MSE)) of the true median home value for the suburb
              
    # Classification Trees ----
                  # Re-coding as binary ----
                    High = ifelse(Carseats$Sales <= 8, "0" ,"1")
                    Carseats = data.frame(Carseats,High)
                    str(Carseats)
                    Carseats$High = as.factor(Carseats$High)

          # Raw tree ----
                  # Fitting and plotting (raw) tree ----
                    tree.carseats = tree(High ~ .-Sales,Carseats, split = c("deviance", "gini"))
                    tree.carseats
                      # how to read it 
                          # root, nr. of obs, deviance, predicted value, propoertion in that branch
                              # repeated for each (...)
                              # if the given variable < ..., then ... (shows same information)
                              # it helps to have the visualization next to it to understand
                          # note the terminal nodes (stars)
                      # Example: Let's pick terminal node labeled “10)”
                          # The splitting variable at this node is X
                          # The splitting value of this node is X 
                          # There are X points in the subtree below this node. 
                          # The deviance for all points contained in region below this node is X 
                          # A star (*) in the line denotes that this is in fact a terminal node. 
                          # The prediction at this node is X=X 
                          # About X% points in this node have X as value of X
                          # Remaining X% points have X as value of X 
                    
                    summary(tree.carseats)
                        # The tree only uses X variables: X and X
                        # It has X terminal nodes. Training error rate (misclassification error) for the tree is X
                          # out of X many predictors, only X are selected
                              # Residual mean deviance:  X = X / X 
                              # Misclassification error rate: X = X / X 
                                  # X predictions in total are wrong
                    plot(tree.carseats)
                    text(tree.carseats,pretty=0)
                        # display labels (text) and category names (pretty)
                        # X is the most important variable of the tree, in fact top X nodes contain X
                        # If X < X, the tree predicts X 
                        # If X > X, the tree predict X 
                        # For intermediate values of X, the decision also depends on the value of X
        
                  # Confusion Matrix ----
                    oj.pred = predict(oj.tree, OJ.test, type = "class")
                    table(OJ.test$Purchase, oj.pred)
                    testerror = (19+32)/(152+19+32+67)
                    testerror
                    
                  # Evaluating raw tree; estimate the test error ----
                    RNGkind("L'Ecuyer-CMRG")
                
                      # Splitting
                        set.seed(2)
                        train=sample(1:nrow(Carseats), 200)
                        Carseats.test = Carseats[-train,]
                        High.test = High[-train]
                      
                      # Fitting on training data
                        tree.carseats = tree(High~.-Sales,Carseats,subset = train)
                      
                      # Predicting with test data
                        tree.pred = predict(tree.carseats,Carseats.test,type = "class")
                        table(tree.pred,High.test)
                        # accuraccy: (87+56)/200 = 0.71; test error = 0.29
                        # it you get slightly different results, it is because of the split
                
          # Pruning tree ----
                  # Pruning tree using CV ----
                  # FUN = prune.misclass
                    RNGkind("L'Ecuyer-CMRG")
                    set.seed(3)
                    cv.carseats = cv.tree(tree.carseats,FUN = prune.misclass) 
                      # FUN: classsification error rate guides the cross-validation and pruning process; default: deviance 
                    names(cv.carseats)
                    cv.carseats
                    # $dev = cross-validation error rate
                    # size = numer of terminal nodes of each tree considered
                    # $k = cost-complexity parameter (alpha)
                
                  # FUN = prune.tree ----
                      # K-fold cross-validation experiment to find the deviance or number of misclassifications 
                      # as a function of the cost-complexity parameter k (alpha, in the textbook).
                          # We determine the optimal tree size not by minimizng RSS (that would maximize purity and overfit)
                          # We instead adjust the hyperparameter alpha (k)
                      set.seed(123)
                      cv.carseats = cv.tree(tree.carseats, FUN = prune.tree) 
                      par(mfrow = c(1, 2))
                      plot(cv.carseats$size, cv.carseats$dev, type = "b", xlab = "Tree Size", ylab = "Deviance") 
                          # we plot the MSE as a function of the number of leaves $size (number of terminal nodes, labelled, T, see p. 309 in textbook)
                          # Size X gives the lowest CV error
                      minor.tick(nx=5)
                      min(cv.carseats$dev)
                          # The minimum cv error is obtained for the size = 9
                          # Alpha parameter can also be seen in 
                            cv.carseats$k 
                      # We can now prune the original tree: 
                        pruned.carseats = prune.tree(tree.carseats, best = 9)
                        par(mfrow = c(1, 1))
                        plot(pruned.carseats)
                        text(pruned.carseats, pretty = 0)
                      # Now use the pruned tree to make prediction on the (correction) test set
                        pred.pruned = predict(pruned.carseats, Carseats.test)
                        mean((Carseats.test$Sales - pred.pruned)^2)
                          # Pruning the tree decreases the test MSE to 4.47 (an inprovement in comparison with the unpruned tree).
                      # Now we can interpret the pruned tree. 
                    
                  # Plotting pruned tree ----
                    par(mfrow=c(1,2))
                    plot(cv.carseats$size,cv.carseats$dev,type="b")
                    plot(cv.carseats$k,cv.carseats$dev,type="b")
                    # this is the most complex model 
                    # meaning that pruning the tree is not useful in this case; 
                    # if pruning is required, as an example, let us choose best = 13
                
                  # selecting size = 13 (instead of 19) 
                    prune.carseats = prune.misclass(tree.carseats,best=13)
                    plot(prune.carseats)
                    text(prune.carseats,pretty=0)
        
                  # Evaluating pruned tree; Estimating test error ----
                    tree.pred=predict(prune.carseats,Carseats.test,type="class")
                    table(tree.pred,High.test)
                      # accuracy: (78+62)/200 =0.7; test error rate = 0.3
                      # here, pruning did not improve the accuracy.
                      # ps. our solution is not the same as in the textbook because of the random split.
                      # the instability of tree solutions when working with small datasets is acknowledged.
       
                  # Comparing pruned to unpruned tree test MSE
                    pred.unpruned = predict(oj.tree, OJ.test, type = "class")
                    table(OJ.test$Purchase, pred.unpruned)
                    testMSEu= (32+19)/(152+19+32+67)
                    # OR
                    misclass.unpruned = sum(OJ.test$Purchase != pred.unpruned)
                    misclass.unpruned/length(pred.unpruned)
                    # Concl: Pruned and unpruned trees have same test error rate of 0.189
                    
                    
                    
    # Ensemble Trees (?) ----

          # Bagging ----
                  # I'm bagging, baggiiing youuu, so put your loving hand out bebeeee
                      
                  # Fitting
                  RNGkind("L'Ecuyer-CMRG")
                  set.seed(1)
                  bag.boston = randomForest(medv ~.,data = Boston,subset = train, mtry = 13, importance = TRUE)
                      # Notice: mtry = p
                      # mtry = nr. predictors; here 14-1 = 13
                      # importance just displays the importance of the variables
                  bag.boston
                  
                  # Evaluating test MSE
                  yhat.bag = predict(bag.boston,newdata=Boston[-train,])
                  plot(yhat.bag, boston.test)
                  abline(0,1)
                  mean((yhat.bag-boston.test)^2)
                  # test MSE for bagging is around 16.37 (significantly lower than that for regression tree)
                  
                  # Comparing; increasing the number of trees grown ntree=1000
                  bag.boston = randomForest(medv~.,data = Boston,subset = train,mtry = 13, ntree=1000)
                      # ntree = how many datasets are created out of the training data, i.e. the number of bags that will be built
                      # how big should it be? just big enough, because eventually the error stagnates.
                      # we will have to wait a long time if we have large data-sets
                  yhat.bag = predict(bag.boston,newdata = Boston[-train,])
                  mean((yhat.bag-boston.test)^2)
                  # [1] 16.06824
                  
                  # Relative Variable Importance
                  importance(bag.boston)
                  varImpPlot(bag.boston)
        
                  # Random Forest ----
                      # Adv.: greatly reduce instability and between-tree correlation
                      # Adv.: faster than bagging 
                      # .......................
                      # Instead of looking at all the predictors, this one selects a random set of predictors 
                      # previously, a lot of similar trees would be generated, because X would always be the most important variable etc.
                      # this way we can take the average, and better reduce variance.
                      # performs better especially with larger data-sets
                      
                      # we mainly rely on the rangers library, as that allows us to control how big we want to build the trees
                
                      RNGkind("L'Ecuyer-CMRG")
                      set.seed(1)
                      
                      # Fitting
                      rf.boston = randomForest(medv~.,data = Boston, subset = train, mtry = 6, importance = TRUE)
                          # how to know where to set mtry 
                                # p/3 for regression (here 10 variables / 3 = around 3)
                                # sqrt(p) for classification (here sqrt(10) would also be around 3)
                                # later we will tune a grid of parameters, e.g. 2, 3, 4 and 5
                          # is mtry = < p?
                          # Changing mtry (m) that is, the number of variables considered at each split, affects test MSE.
                      
                      # Evaluating test MSE 
                      yhat.rf = predict(rf.boston,newdata = Boston[-train,])
                      mean((yhat.rf-boston.test)^2)
                      # [1] 15.53946
                      # --> RF yielded an improvement over bagging
                      
                      # Relative Variable Importance
                        importance(rf.boston)
                        varImpPlot(rf.boston)
                  
                          # Variable Importance ----
                        importance(rf.boston)
                        varImpPlot(rf.boston)
                        # Concl. across all the trees considered by the random forest, 
                        # the wealth level of community and the house size (rm)
                        # are by far the two most important variables. 
                        
                          # Alternative: automatically computing the test MSE as a function of the number of trees ---- 
                          # Via the use of X.train, X.test, T.train and T.test as matrices to
                          
                          # We use an alternate call to randomForest() which uses X.train, X.test, Y.train and Y.test as matrices. 
                          # This approach allows us to automatically compute test MSE as a function of number of trees
                        
                          # Split; Constructing of train and test matrices
                            set.seed(1101)
                            train = sample(dim(Boston)[1], dim(Boston)[1]/2) # 50/50 split
                            X.train = Boston[train, -14] # DV is medv (column 14)
                            X.test = Boston[-train, -14]
                            Y.train = Boston[train, 14]
                            Y.test = Boston[-train, 14]
                        
                          # Setting the parameters for random forest 
                            # ntree (B in the theory): I will try a range of ntree from 1 to 500. 
                            # ntree = how many datasets are created out of the training data, 
                                # i.e. the number of bags that will be built
                                # how big should it be? just big enough, because eventually the error stagnates.
                                # we will have to wait a long time if we have large data-sets
                            #  For mtry I take the three typical values:  
                                # p (this is a bagging model; p is number of IV)
                                # p/2
                                # sqrt(p)
                                  p = dim(Boston)[2] - 1 # recall, p is number of IV 
                                  p.2 = p/2
                                  p.sq = sqrt(p)
                            
                          # Fitting with mtry parameters
                              rf.boston.p = randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                                                         mtry = p, ntree = 500) # mtry = p is simply a bagging model
                              rf.boston.p.2 = randomForest(X.train, Y.train,xtest = X.test, ytest = Y.test, 
                                                           mtry = p.2, ntree = 500)
                              rf.boston.p.sq = randomForest(X.train, Y.train,xtest = X.test, ytest = Y.test, 
                                                            mtry = p.sq, ntree = 500)
                          
                          # Training and test MSE, and training and test OOB
                            rf.boston.p
                              # training MSE: 16.68  
                              # % Var Explained = 1 - RSS/TSS= 81.56 for training random forest (a particular case of bagging) aka "Out of the Bag error (OOB)"
                              # MSE for test: 11.73
                              # and % var explained for test : 84.98
                            rf.boston.p.2 
                            rf.boston.p.sq
                            
                          # Plotting OOB error in bagging is the training error
                            plot(1:500, rf.boston.p$mse, col = "green", type = "l", xlab = "Number of Trees", 
                                 ylab = "OOB-MSE", ylim = c(10, 25))
                            lines(1:500, rf.boston.p.2$mse, col = "red", type = "l")
                            lines(1:500, rf.boston.p.sq$mse, col = "blue", type = "l")
                            legend("topright", c("m=p", "m=p/2", "m=sqrt(p)"), col = c("green", "red", "blue"), 
                                   cex = 1, lty = 1)
                            minor.tick(nx=10)
                            # NOTES
                              # OOB error is a good estimator for the testing error;
                              # if ntree is sufficiently large, OOB error is virtually equivalent to leave-one-out cross validation error 
                              # we observe in the plot the trend of OOB-MSE is similar to the trend of test error.
                            
                          # Obtaining Test MSE of these models by accessing mse list member of the test model
                            rf.boston.p$test$mse
                            rf.boston.p.2$test$mse
                            rf.boston.p.sq$test$mse
                            
                          # Plotting the test error as a function of the number of trees used
                            plot(1:500, rf.boston.p$test$mse, col = "green", type = "l", xlab = "Number of Trees", 
                                 ylab = "Test MSE", ylim = c(10, 25))
                            lines(1:500, rf.boston.p.2$test$mse, col = "red", type = "l")
                            lines(1:500, rf.boston.p.sq$test$mse, col = "blue", type = "l")
                            legend("topright", c("m=p", "m=p/2", "m=sqrt(p)"), col = c("green", "red", "blue"), 
                                   cex = 1, lty = 1)
                            minor.tick(nx=10)
                            # Test MSE for single tree is quite high. 
                            # It is reduced by adding more trees to the model and stabilizes around a few hundred trees. 
                            # Test MSE for including all variables at split (bagging) is slightly higher than test MSE using half or square-root number of variables. 
                            # Overall, for these data random forest perform better than bagging. 
                            
                          # Other alternative: Using caret library to tune "mtry" ----
                              # PS: "ntree" is not tuned as it is not a critical parameter with bagging. 
                                # Using a very large value for ntree will not lead to overfitting 
                                  set.seed(123456)
                                  train <- cbind(X.train, Y.train)
                                  tune.grid <- data.frame(.mtry = c(1:13))
                                  train.param <- trainControl(method = "cv", number = 5)
                                  rf.boston <- train(Y.train ~., train, 
                                                     method = "rf", 
                                                     ntree = 500, 
                                                     trControl = train.param, 
                                                     tuneGrid = tune.grid)
                                  rf.boston
                                  # CV-RMSE for mtry = 6 is the minimum
                                      # for updates in rf function
                                      # rfNews()
                            
                            # test MSE 
                              test = Boston[-train,]
                              yhat.rf = predict(rf.boston,newdata = Boston[-train,])
                              mean((yhat.rf - test$medv)^2)
                              #[1] 9.45507
                            
                            # variable importance
                              vip::vip(rf.boston, num_features = 40, bar = FALSE)
                          
                          # Partial Dependence Plots (PDPs) ----
                            p1 <- pdp::partial(
                              rf.boston, 
                              pred.var = "lstat",
                              grid.resolution = 20
                            ) %>% 
                              autoplot()
                            
                            p2 <- pdp::partial(
                              rf.boston, 
                              pred.var = "rm", 
                              grid.resolution = 20
                            ) %>% 
                              autoplot()
                            
                            gridExtra::grid.arrange(p1, p2, nrow = 1)
                            
                  # ranger library ----
                      
                      # Splitting data
                        set.seed(123)
                        ames <- AmesHousing::make_ames()
                        set.seed(123)
                        split <- initial_split(ames, prop = 0.7, 
                                               strata = "Sale_Price")
                        ames_train  <- training(split)
                        ames_test   <- testing(split)
                      
                      # Number of features
                        n_features <- length(setdiff(names(ames_train), "Sale_Price"))
                        n_features
                        
                      # train a default random forest model using ranger library
                        ames_rf1 <- ranger(
                          Sale_Price ~ ., 
                          data = ames_train,
                          mtry = floor(n_features / 3), # p/3 rule (for regressions)
                          respect.unordered.factors = "order",
                          seed = 123
                        )
                            # Here we set the number of trees as default. so we do not control it
                            # we can control this by defining a grid (cf. below)
                        
                        # get Out-of-bag RMSE (OOB RMSE)
                          (default_rmse <- sqrt(ames_rf1$prediction.error))
                      
                      # Controlling the number of trees 
                        
                        # create hyperparameter grid
                          hyper_grid <- expand.grid(
                            mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
                            min.node.size = c(1, 3, 5, 10), 
                            replace = c(TRUE, FALSE),                             
                            sample.fraction = c(.5, .63, .8),                       
                            rmse = NA                                               
                          )
                              # this does the split at each size until we have at least, 1, then at least 3, 5 and 10 observations at each node
                              # we can set whether we want with or without replacement (T/F)
                              # sample.fraction allows us to impose a maximum (e.g. 0.5, i.e. 50%) of each variable when making the split
                      
                        # execute full cartesian grid search (where we assess every combination of hyper-parameters of interest) 
                              # for each element in the sequence we run the model, where we regress sales price on all the variables etc.
                          
                          for(i in seq_len(nrow(hyper_grid))) {
                            # fit model for ith hyperparameter combination
                            fit <- ranger(
                              formula         = Sale_Price ~ ., 
                              data            = ames_train, 
                              num.trees       = n_features * 10, # how many bags to create
                              mtry            = hyper_grid$mtry[i],
                              min.node.size   = hyper_grid$min.node.size[i],
                              replace         = hyper_grid$replace[i],
                              sample.fraction = hyper_grid$sample.fraction[i],
                              verbose         = FALSE,
                              seed            = 123,
                              respect.unordered.factors = 'order',
                            )
                            
                            # export OOB error 
                            hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
                          }
                      
                          # assessing top 10 models ----
                            hyper_grid %>%
                              arrange(rmse) %>%
                              mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
                              head(10)
                              # the top model has mtry of 26 (out of 80) with etc. etc. parameters
                              # we can then take these and implement it (make predictions etc.)
                          
                          # Feature importance ----
                                  # re-run model with impurity-based variable importance ----
                                    rf_impurity <- ranger(
                                      formula = Sale_Price ~ ., 
                                      data = ames_train, 
                                      num.trees = 2000,  # notice the model is re-run with the optimal hyperparam identified before
                                      mtry = 32,
                                      min.node.size = 1,
                                      sample.fraction = .80,
                                      replace = FALSE,
                                      importance = "impurity",  # based on impurity
                                      respect.unordered.factors = "order",
                                      verbose = FALSE,
                                      seed  = 123
                                    )
                              
                                  # re-run model with permutation-based variable importance ----
                                  rf_permutation <- ranger(
                                    formula = Sale_Price ~ ., 
                                    data = ames_train, 
                                    num.trees = 2000,
                                    mtry = 32,
                                    min.node.size = 1,
                                    sample.fraction = .80,
                                    replace = FALSE,
                                    importance = "permutation",  # based on permutation
                                    respect.unordered.factors = "order",
                                    verbose = FALSE,
                                    seed  = 123
                                  ) 
                                  
                                  # Plotting ----
                                  # Typically, you will not see the same variable importance order between the two options.
                                  # however, you will often see similar variables at the top of the plots (and also the bottom). 
                                  
                                  p1 <- vip::vip(rf_impurity, num_features = 25, bar = FALSE)
                                  p2 <- vip::vip(rf_permutation, num_features = 25, bar = FALSE)
                                  
                                  gridExtra::grid.arrange(p1, p2, nrow = 1)
                        
                              
                              
          # Boosting ---- 
                  # Continuous DV ----
                  
                          # Fitting
                            set.seed(1)
                            boost.boston = gbm(medv ~ ., data = Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
                            summary(boost.boston)
                            
                          # Plotting
                            par(mfrow=c(1,2))
                            plot(boost.boston,i="rm")
                            plot(boost.boston,i="lstat")
                            
                          # Evaluating test MSE
                            yhat.boost = predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
                            mean((yhat.boost - boston.test)^2)
                            # [1] 15.83297 (MSE) similar to Rf
                            
                            # Comparing; Changing the shrinkage parameter 
                              boost.boston = gbm(medv~.,data = Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
                              yhat.boost = predict(boost.boston,newdata = Boston[-train,], n.trees = 5000)
                              mean((yhat.boost - boston.test)^2)
                              # [1] 19.15354 (MSE) --> changing the shrinkage parameter does not imporve the fit in this case. 
                              # creating a grid of values for this parameter and testing those values may help to identify the best fit. 
              
                            # Shrinkage Parameter, λ ----
                              # Training a set with 1000 trees for a range of values of the shrinkage parameter, λ. ----
                                set.seed(103)
                                pows = seq(-10, -0.2, by = 0.1)
                                lambdas = 10^pows
                              
                                length.lambdas = length(lambdas)
                                train.errors = rep(NA, length.lambdas)
                                test.errors = rep(NA, length.lambdas)
                              
                                for (i in 1:length.lambdas) {
                                  boost.hitters = gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", 
                                                      n.trees = 1000, shrinkage = lambdas[i])
                                  train.pred = predict(boost.hitters, Hitters.train, n.trees = 1000)
                                  test.pred = predict(boost.hitters, Hitters.test, n.trees = 1000)
                                  train.errors[i] = mean((Hitters.train$Salary - train.pred)^2)
                                  test.errors[i] = mean((Hitters.test$Salary - test.pred)^2)
                                }
                              
                              # Plotting different shrinkage values on x-axis with corresponding training set MSE on y-axis ----
                                plot(lambdas, train.errors, type = "b", xlab = "Shrinkage", ylab = "Train MSE", 
                                   col = "blue", pch = 20)
                              
                              # Plot different shrinkage values on x-axis with corresponding TEST set MSE on y-axis ----
                                
                                plot(lambdas, test.errors, type = "b", xlab = "Shrinkage", ylab = "Test MSE", 
                                     col = "red", pch = 20)
                                
                                min(test.errors) 
                                ## [1] 0.2560
                                
                                lambdas[which.min(test.errors)] 
                                ## [1] 0.05012. Minimum test error is obtained at λ=0.05
                  
                              # Using caret library to tune the parameters ----
                                  # for boosting, we have 3-4 main parameters:
                                      # n.trees, B
                                      # shrinkage, lambda
                                      # interaction.depth, d
                                      # + n.minobsinnode (Min Teminal Node Size)
                                        set.seed(123)
                                        library(caret)
                                
                                        train.param <- trainControl(method = "cv", number=5)
                                        tune.grid <- expand.grid(n.trees = (0:50)*50, interaction.depth = c(1, 3, 5), shrinkage = lambdas, n.minobsinnode=10)
                                        
                                        boost.hitters.caret <- train(Salary ~., data = Hitters.train, 
                                                                     method = "gbm", 
                                                                     trControl=train.param,
                                                                     tuneGrid=tune.grid)
                                        boost.hitters
                                  
                                        test.pred = predict(boost.hitters.caret, Hitters.test, n.trees = # ) # requires to be defined based on the model above 
                                                              testMSE = mean(Hitters.test$Salary - test.pred)^2)
                                        testMSE
                                  
                                        # Comparing with Linear Regression with Lasso ----
                                        
                                            # Applying linear regression
                                              lm.fit = lm(Salary ~ ., data = Hitters.train)
                                              lm.pred = predict(lm.fit, Hitters.test)
                                              mean((Hitters.test$Salary - lm.pred)^2)
                                              ## [1] 0.4918
                                            
                                            # Applying lasso
                                              library(glmnet)
                                              set.seed(134)
                                              x = model.matrix(Salary ~ ., data = Hitters.train)
                                              y = Hitters.train$Salary
                                              x.test = model.matrix(Salary ~ ., data = Hitters.test)
                                              lasso.fit = glmnet(x, y, alpha = 1)
                                              lasso.pred = predict(lasso.fit, s = 0.01, newx = x.test)
                                              mean((Hitters.test$Salary - lasso.pred)^2)
                                              ## [1] 0.4701
                                            
                                            # Concl: Both linear model and regularization like Lasso have higher test MSE than boosting.
                                  
                                        # Variable Importance ----
                                            boost.best = gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", 
                                                             n.trees = 1000, shrinkage = lambdas[which.min(test.errors)])
                                            summary(boost.best)
                                            # Concl: CAtBat, CRBI and CWalks are the three most important variables. 
                                
                  # Binary DV ----
                    # Fitting
                      library(ISLR)
                      train = 1:1000
                      Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0) # recode classes as 1 and 0; where 1 = customer purchased a caravan insurance policy; 0=otherwise
                      Caravan.train = Caravan[train, ]
                      Caravan.test = Caravan[-train, ]
                      
                      library(gbm)
                      set.seed(342)
                      boost.caravan = gbm(Purchase ~ ., data = Caravan.train, n.trees = 1000, shrinkage = 0.01, 
                                          distribution = "bernoulli")
                      summary(boost.caravan)
                      
                      # Concl: PPERSAUT, MKOOP KLA and MOPLHOOG are three most important variables
                      
                              # Plotting variable importance ----
    
                        summary(Caravan.train$PPERSAUT)
                        summary(Caravan.train$MKOOPKLA)
                        summary(Caravan.train$MOPLHOOG)
                        
                        plot(boost.caravan, i="PPERSAUT")
                        plot(boost.caravan, i="MKOOPKLA")
                        plot(boost.caravan, i="MOPLHOOG")
                    
                              # Predicting ----
                                  
                                  # Predict that a person will make a purchase if the estimated probability of purchase is greater than 20 %. 
                                  # Form a confusion matrix. What fraction of the people predicted to make a purchase do in fact make one? 
                                  # How does this compare with the results obtained from applying KNN or logistic regression to this data set?
                                  
                                  boost.prob = predict(boost.caravan, Caravan.test, n.trees = 1000, type = "response") 
                                      # note we use type="response" to retain probabilities 
                                      # also, we need to mention again "n.trees = 1000". 
                                  boost.pred = ifelse(boost.prob > 0.2, 1, 0)
                                  table(Caravan.test$Purchase, boost.pred)
                                  34/(137 + 34) 
                                      # Precision (TP/P*) is 0.1988. Thus, about 20% of people predicted to make purchase actually end up making one.
                                
                              # Comparing Prediction to Logistic Regression Predictions ----
                                  
                                  # Logistic regression 
                                  lm.caravan = glm(Purchase ~ ., data = Caravan.train, family = binomial)
                                  lm.prob = predict(lm.caravan, Caravan.test, type = "response")
                                  lm.pred = ifelse(lm.prob > 0.2, 1, 0)
                                  table(Caravan.test$Purchase, lm.pred)
                                  
                                  58/(350 + 58)
                                  ## [1] 0.1422 -> About 14 % of people predicted to make purchase using logistic regression actually end up making one. 
                                  # The precision is lower than for boosting.
                                  
                                  # Note: In applications one can use other popular performance measures (AUC,recall, ...) 
                                  # as discussed in the first part of the curriculum (ISL p. 147-149). 
                        
                  # rpart ----
                              # method != rpart --> WITHOUT cp-based pruning ----
                              
                                  # Splitting
                                    ames <- AmesHousing::make_ames()
                                    set.seed(123)
                                    split <- initial_split(ames, prop = 0.7, 
                                                           strata = "Sale_Price")
                                    ames_train  <- training(split)
                                    ames_test   <- testing(split)
                                  
                                  # Fitting 
                                    ames_dt1 <- rpart(
                                      formula = Sale_Price ~ .,
                                      data    = ames_train,
                                      method  = "anova" # if we do not write that, it will run a classification, which does not work with continuous variables
                                    )
                                    ames_dt1
                                    # the stars are the terminal nodes
                                  
                                  # Plotting tree
                                    rpart.plot(ames_dt1)
                                      # this also shows the proportions
                                      # it also shows the nodes with the highest values (dark blue)
                                  
                                  # Variable Importance
                                    par(mar=c(8,4,3,2))
                                    d <- as.data.frame(ames_dt1$variable.importance)
                                    varImpPlot(ames_dt1)
                                  
                                  # plot cp (complexity parameter, alpha)
                                    plotcp(ames_dt1) 
                                      # it decreases until it reaches the optimum (dotted lines)
                                  
                                  # rpart cross validation results
                                    ames_dt1$cptable
                                      # identifies the minimum relative error (and associated CV); #7
                                  
                                  # Comparing; if cp=0
                                    ames_dt2 <- rpart(
                                      formula = Sale_Price ~ .,
                                      data    = ames_train,
                                      method  = "anova", 
                                      control = list(cp = 0, xval = 10)
                                    )
                                    plotcp(ames_dt2)
                                    abline(v = 11, lty = "dashed")
                
                              # "Method = rpart" --> WITH cp-based pruning ----
                                    
                                    # Fitting
                                    ames_dt3 <- train(
                                      Sale_Price ~ .,
                                      data = ames_train,
                                      method = "rpart",
                                      metric = "RMSE",
                                      trControl = trainControl(method = "cv", number = 10),
                                      tuneLength = 20 
                                    )
                                    ames_dt3
                                    
                                    # Plotting model
                                    ggplot(ames_dt3)
                                    
                                    # Optimal cp (complexity parameter; alpha) 
                                    ames_dt3$bestTune
                                        # Lowest cp is optimal
                                    
                                    # Plotting tree
                                      ames_dt3$finalModel
                                      fancyRpartPlot(ames_dt3$finalModel, sub = NULL)
                                    
                                    # Variable Importance
                                      vip(ames_dt3, num_features = 40, bar = FALSE)
                                    
                                    # Partial dependence plots (Takes a while to load)
                                      p1 <- partial(ames_dt3, pred.var = "Gr_Liv_Area") %>% autoplot()
                                      p2 <- partial(ames_dt3, pred.var = "Year_Built") %>% autoplot()
                                      p3 <- partial(ames_dt3, pred.var = c("Gr_Liv_Area", "Year_Built")) %>% 
                                        plotPartial(levelplot = FALSE, zlab = "yhat", drape = TRUE, colorkey = TRUE, 
                                                    screen = list(z = -20, x = -60))
                                        # Displaying plots side by side
                                        gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
                                        # notice decision trees have rigid non-smooth prediction surfaces compared to MARS
                                    
                              # "method = rpart2" --> max tree depth tuning (tree selection based on depth) ----
                                        ames_dt4 <- train(
                                          Sale_Price ~ .,
                                          data = ames_train,
                                          method = "rpart2",
                                          metric = "RMSE",
                                          trControl = trainControl(method = "cv", number = 10),
                                          tuneLength = 20 #(max. 20)
                                        )
                                        ames_dt4
                                        ggplot(ames_dt4)
                                        ames_dt4$bestTune
                                        # best depth is 12 here.
                                        
                                        ames_dt4$finalModel
                                        fancyRpartPlot(ames_dt4$finalModel, sub = NULL)
                                    
                              # Comparing test RMSE of above models ----
                                      # rpart library with rpart function, cp-based pruning
                                        dt1_pred = predict(ames_dt1, ames_test)
                                        dt1_RMSE = sqrt(mean((ames_test$Sale_Price - dt1_pred)^2))
                                        dt1_RMSE
                                      
                                      # caret with "method = rpart", cp-based pruning
                                        dt3_pred = predict(ames_dt3, ames_test)
                                        dt3_RMSE = sqrt(mean((ames_test$Sale_Price - dt3_pred)^2))
                                        dt3_RMSE
                                      
                                      # caret with "method = rpart2",max tree depth tuning 
                                        dt4_pred = predict(ames_dt4, ames_test)
                                        dt4_RMSE = sqrt(mean((ames_test$Sale_Price - dt4_pred)^2))
                                        dt4_RMSE
                                        # Pruning based on alpha (cp) seems best
                

                        
                
              
              
              
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        