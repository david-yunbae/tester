#' Assumption
#'
#' @description This function outputs the 3 graphs of assumption for the linear regression model.
#' @param x text string; This should be the data file name of the test sample.
#'
#' @return 3 graphs of linearity, constant variance and normal distribution of the residuals
#' @export
assumption <- function(x,testtype){
  dat <- readr::read_csv(x,show_col_types = FALSE)

  #Prepare assumption for lm
  D <- lm(height ~ weight,dat)
  a <- ggplot2::ggplot(dat,aes(x=height,y=weight))+ geom_point() + stat_smooth(method="lm", col="red") +ggtitle("I) Y vs X")

  b <- ggplot2::ggplot(dat)+geom_point(mapping=aes(x=D$fitted.values ,y=D$residuals)) + geom_hline(yintercept=0,lwd=2)+ggtitle("II) Residual plot")+ylab("Residuals")+xlab("Fitted values")

  c <- ggplot2::ggplot(dat)+geom_histogram(mapping=aes(x=D$residuals),bins=40) +ggtitle("III) Distribution is normal")+xlab("Residuals")

  #Prepare assumption for ttest
  d <- ggplot2::ggplot(dat, aes(sample=height, group=gender, colour=gender))+geom_qq()+geom_qq_line()+xlab("theoretical")+ylab("sample")

  e <- dat %>% group_by(gender) %>% summarise(n=n(),mu=mean(height),sd=sd(height))

  #Preapre assumption for chitest
  datm <- dat %>% filter(gender=="Male") %>% select(phys)
  datf <- dat %>% filter(gender=="Female") %>% select(phys)

  datmn <- datm %>% filter(phys=="None") %>% count()
  datmm <- datm %>% filter(phys=="Moderate") %>% count()
  datmi <- datm %>% filter(phys=="Intense") %>% count()

  datfn <- datf %>% filter(phys=="None") %>% count()
  datfm <- datf %>% filter(phys=="Moderate") %>% count()
  datfi <- datf %>% filter(phys=="Intense") %>% count()

  table <- dplyr::tibble(Male=c(datmn[[1]],datmm[[1]],datmi[[1]]),Female=c(datfn[[1]],datfm[[1]],datfi[[1]]))

  cat("STAGE II>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Assumptions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> \n")
  Sys.sleep(2)

  if (testtype=="lm"){
    cat("Assumptions: \n \nI Linearity\nII Constant variance\nIII Residuals normally distributed\n \n")
    print((a+b)/c)
  } else if (testtype=="ttest"){
    cat("Assumptions: \n \nI Normality\nII Equal variance (slarger/ssmaller < 2) - Actual value: ", max(e$sd)/min(e$sd),"\n \n")
    print(d)
  } else if (testtype=="chitest"){
    cat("Assumptions: \n \nI Normal approximation: All entries must be at least 5.\n \n")
    print(table)
  }
}

wrapperlm <- function(x){
  dat <- readr::read_csv(x,show_col_types = FALSE)
  B <- lm(height~weight,dat)$coefficients[[2]]
  df <- lm(height~weight,dat)$df.residual
  p <- broom::glance(lm(height~weight,dat))$p.value[[1]]
  SE <- coef(summary(lm(height~weight,dat)))[,"Std. Error"][[2]]

  out <- list(type="lm",param=x,beta=B,CI=t*SE,t_value=t,degree_of_freedom=df,p_value=p)
  out$CI <- list(min=B-t*SE,max=B+t*SE)

  class(out) <- "myr"
  out
}

wrapperttest <- function(x){
  dat <- readr::read_csv(x,show_col_types = FALSE)
  datm <- dat %>% filter(gender=="Male") %>% select(height)
  datf <- dat %>% filter(gender=="Female") %>% select(height)

  test <- t.test(datm,datf,var.equal=TRUE)
  out <- list(type="ttest",param=x,CI=t,t_value=test[[1]][[1]],degree_of_freedom=test[[2]][[1]],p_value=test[[3]][[1]])
  out$CI <- list(min=broom::glance(test)$conf.low,max=broom::glance(test)$conf.high)

  class(out) <- "myr"
  out
}

wrapperchitest <- function(x){
  dat <- readr::read_csv(x,show_col_types = FALSE)
  datm <- dat %>% filter(gender=="Male") %>% select(phys)
  datf <- dat %>% filter(gender=="Female") %>% select(phys)

  datmn <- datm %>% filter(phys=="None") %>% count()
  datmm <- datm %>% filter(phys=="Moderate") %>% count()
  datmi <- datm %>% filter(phys=="Intense") %>% count()

  datfn <- datf %>% filter(phys=="None") %>% count()
  datfm <- datf %>% filter(phys=="Moderate") %>% count()
  datfi <- datf %>% filter(phys=="Intense") %>% count()

  table <- dplyr::tibble(Male=c(datmn[[1]],datmm[[1]],datmi[[1]]),Female=c(datfn[[1]],datfm[[1]],datfi[[1]]))

  test <- chisq.test(table,correct=FALSE)
  out <- list(type="chitest",param=x,t_value=test[[1]][[1]],degree_of_freedom=test[[2]][[1]],p_value=test[[3]][[1]])

  class(out) <- "myr"
  out
}

printer.myr <- function(x){
  if(x$type=="lm"){
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>> Relevant test: Linear regression <<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n \n")
    Sys.sleep(4)
    cat("STAGE I>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Hypothesis >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n \nH0 : B = 0 against H1 : B != 0\n \n")
  } else if(x$type=="ttest"){
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>> Relevant test: t test <<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n \n")
    Sys.sleep(4)
    cat("STAGE I>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Hypothesis >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n \nH0 : mu1 = mu2 against H1 : mu1 != mu2\n \n")
  } else if(x$type=="chitest"){
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>> Relevant test: chi test <<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n \n")
    Sys.sleep(4)
    cat("STAGE I>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Hypothesis >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n \nH0 : the two variables are independent against each other. H1 : not H0.\n \n")
  }

  Sys.sleep(2)
  assumption(x$param,x$type)

  Sys.sleep(4)
  cat("STAGE III>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Results >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n \n")

  if(x$type=="lm"){
    cat("I beta: ", x$beta, "\nII CI: (", x$CI$min, ",", x$CI$max, ")\nIII degree of freedom: ", x$degree_of_freedom, "\nIV critical value : ", x$t_value, "\nV p value: ", x$p_value, "\n \n")
  } else if(x$type=="ttest"){
    cat("I CI: (", x$CI$min, ",", x$CI$max, ")\nII degree of freedom: ", x$degree_of_freedom, "\nIII critical value : ", x$t_value, "\nIV p value: ", x$p_value, "\n \n")
  } else if(x$type=="chitest"){
    cat("I degree of freedom: ", x$degree_of_freedom, "\nII critical value : ", x$t_value, "\nIII p value: ", x$p_value, "\n \n")
  }

  Sys.sleep(5)
  cat("STAGE IV>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Decision >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n \n")
  Sys.sleep(3)
  if (x$p_value < 0.05){
    cat("REJECT: p-value = ", x$p_value, " < 0.05\n \n")
  } else {
    cat("DO NOT REJECT: p-value = ", x$p_value, " > 0.05\n \n")
  }

  Sys.sleep(4)
  cat("STAGE V>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Conclusion >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n \n")
  if (x$type=="lm"){
    if (x$p_value <0.05){
      cat("There is a relationship between height and weight: As the P-value is very small, we have very strong evidence to reject H0. I.E. very strong evidence that the slope parameter is significant and there is a relationship between the height and weight of the sample population.")
    } else if(x$p_value >0.05){
      cat("There isn't any relationship between height and weight: As the P-value is large, we have no evidence to reject H0. I.E. no evidence that the slope parameter is significant and there isn't any relationship between the height and weight of the sample population.")
    }
  } else if(x$type=="ttest"){
    if (x$p_value <0.05){
      cat("The mean height of male and female are NOT the same: As the P-value is very small, we have very strong evidence to reject H0. I.E. very strong evidence that the mean height of male is not the same as the mean height of female.")
    } else if(x$p_value >0.05){
      cat("The mean height of male and female are the same: As the P-value is large, we have no evidence to reject H0. I.E. no evidence that the mean height of male is not the same as the mean height of female.")
    }
  } else if(x$type=="chitest"){
    if (x$p_value <0.05){
      cat("Gender affects the amount of physical activity: As the P-value is very small, we have very strong evidence to reject H0. I.E. very strong evidence that the two variables are dependent against each other. Gender affects the physical activity.")
    } else if(x$p_value >0.05){
      cat("Gender does NOT affect the amount of physical activity: As the P-value is large, we have no evidence to reject H0. I.E. no evidence that the two variables are dependent against each other. The two variables are independent against each other and there is no association between gender and the amount of physical acitivity.")
    }
  }
}

printer <- function(x){
  UseMethod("printer")
}
