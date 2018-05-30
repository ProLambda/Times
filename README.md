**Every fork can change the GPL license to other**

### Introduction ###

A Hacker-News-like social information platform written in Haskell, focusing on IT industry and computer science, which users could publish general news, academic contents and questions through it. Reducing time wasting on nonnutritive information is the major goal

[Chinese blog article](http://izhen.me/2017/08/20/aws-lambda/)

### Version ###

0.2.8

### First Launch ###

Please configure following settings before your first launch:

1. Configure postgre server, and set dbname/pass/etc. in Skeleton/Kernel/Internal/Model.hs

2. Set email account in Skeleton/Kernel/Core/Mail.hs

3. Put dist of ProLambda/Times-default-theme into static folder

4. `$stack build` 

### Demo ###

#### Submit ####

![](https://github.com/ProLambda/Times/blob/master/submit.gif?raw=true)

#### Comment ####

![](https://github.com/ProLambda/Times/blob/master/view.gif?raw=true)

#### Full Demo on Youtube ####

[Click Here](https://www.youtube.com/watch?v=ZJfdvKRax1Q)

### Design ###

![](https://github.com/ProLambda/Times/blob/master/lambda-design.png?raw=true)
