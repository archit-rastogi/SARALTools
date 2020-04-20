
/* rubrik agil expert predicates
  
  resource types => executor, cluster, polaris, snappables
  resource => type + settings

  feature => name + location
  test => feature + sub-directory + filename + classname
  test case => test method name + test
  test case resources => minimum resources required to run all tests 
  test case tags => test case + tags
  test case metadata => test case + description + steps

  suite => name + description
  suite_tests => name + tests
   
  repos_type => type + url
  repository => repos_type + name + credentials
  release => repository + branch + tag

  config type => runner + name
  config => config type + settings
  profile => profile_type + config_list
   
  runner => runner name + executable
  runner run configuration => env details + profile
  
  test case run configuration => test case + runner run coniguration + profile

  role => name + type + settings
  technician => name + roles
   
*/

:- module(rkagiletestexp, []).

:- dynamic
        feature/2,
        classname_location/2,
        test_classname/2,
        test_resources/2,
        test_case/2,
        test_case_resources/2.

/** move appropriate module.

  triage(Filename, TestResult)
  **/

:- dynamic
        triage

triage(run_log(FileName), TestResult) :- true.

   


       