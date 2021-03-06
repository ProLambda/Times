### Introduction ###

A Hacker-News-like social information platform written in Haskell, focusing on IT industry and computer science, which users could publish general news, academic contents and questions through it. Reducing time wasting on nonnutritive information is the major goal

[Demo](http://ec2-18-182-66-26.ap-northeast-1.compute.amazonaws.com)

[Chinese blog article](http://izhen.me/2017/08/20/aws-lambda/)

### Version ###

0.2.9

### First Launch ###

Please configure following settings before your first launch:

0. Set 'allow-newer: true' in ~/.stack/config.yaml

1. Configure postgre server, and set dbname/pass/etc. in Skeleton/Kernel/Internal/Model.hs

2. Set email account in Skeleton/Kernel/Core/Mail.hs

3. Put dist of ProLambda/Times-default-theme into static folder

4. -- initializeStatDb

   -- initializeNewsDb
   
   -- setLevel "xxx" 12 (+1) -- "xxx" could be any registered user, for setting super admin, can be run when has at least one user
   
   uncomment up 3 lines in Main.hs, this is necessary init step, then do following
   ```
   $stack build
   $stack exec proj
   ```

5. Comment all 3 lines in step 4 and then `$stack build` again
6. For DISQUS configuration, please check relative JS code in main.html
7. For PostgreSQL setting, please refer to official doc
8. create 'database' and 'imgs' folder in root dir

### Demo ###

#### Submit ####

![](https://github.com/ProLambda/Times/blob/master/submit.gif?raw=true)

#### Comment ####

![](https://github.com/ProLambda/Times/blob/master/view.gif?raw=true)

#### Full Demo on Youtube ####

[Click Here](https://www.youtube.com/watch?v=ZJfdvKRax1Q)

### Design ###

![](https://github.com/ProLambda/Times/blob/master/lambda-design.png?raw=true)
