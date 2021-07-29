# Pull Request Summary
(Instructions: this, and all subsequent sections of text should be removed and filled in as appropriate.)   
Please describe the PR summary

## Description
Provide a detailed description of what this PR does.
What bug does it fix, or what feature does it add?
Is a change of answers expected from this PR?

### Issue(s) addressed
* Is there an issue associated with this development (bug fix, enhancement, new feature)?    
Please add a reference to a related issue(s) in WW3 repository (Follow [link](https://docs.github.com/en/github/managing-your-work-on-github/linking-a-pull-request-to-an-issue)).
Link the issues to be closed with this PR, whether in this repository, or in another repository.
(Remember, issues should always be created before starting work on a PR branch!).  
Note that properly "linked issues" (either automatic links, or manual ones using the correct keywords) will be automatically closed when the PR is merged.

- fixes #<issue_number>
- fixes noaa-emc/ww3/issues/<issue_number>

### Check list  
* Is your feature branch up to date with the authoritative repository (NOAA/develop)?
* Make sure you have checked the [checklist for a developer submitting to develop](https://github.com/NOAA-EMC/WW3/wiki/Code-Management#checklist-for-a-developer-submitting-to-develop) and [updating version number](https://github.com/NOAA-EMC/WW3/wiki/Code-Management#checklist-for-updating-version-number)
* Please list appropriate labels code managers should add for this PR:   
 _bug_, _documentation_, _enhancement_, _new feature_, ..
 
* Reviewers: @mentions of suggested reviewers of the proposed changes

### Commit Message
* Please provide a short summary of this PR, which will be used during _Squash and Merge_ and will be shown as git log message 

### Testing
* How were these changes tested?
* Are the changes covered by regression tests? (If not, why? Do new tests need to be added?)
* If a new feature was added, was a new regression test added?
* Have regression tests been run?
* Which compiler / HPC you used to run the regression tests in the PR? 
* Please provide the summary output of matrix.comp (_matrix.Diff.txt_, _matrixCompFull.txt_ and _matrixCompSummary.txt_):    
Please indicate the expected changes in the outputs ([excluding the known list of non-identical tests](https://github.com/NOAA-EMC/WW3/wiki/How-to-use-matrix.comp-to-compare-regtests-with-master#4-look-at-results)).
* Please list which labels code managers should add to indicate code changes:    
_mod_def change_, _out_grd change_, _out_pnt change_, _restart file change_, _Regression test_, ...

