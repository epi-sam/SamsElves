test_that(
  "submit_job input validation produces expected errors",
  {
    expect_error(
      submit_job(
        language               = "R",
        shell_script_path      = NULL, 
        script_path            = NULL, 
        std_err_path           = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
        std_out_path           = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
        job_name               = NULL, 
        archiveTF              = FALSE,  
        mem_GB                 = "10G", 
        threads                = "2", 
        runtime_min            = "15", 
        partition              = "all.q", 
        Account                = NULL, 
        r_image                = NULL,  
        args_list              = NULL,
        dry_runTF              = TRUE
      ),
      regexp = "Please define a valid script path to submit"
    )
    
    expect_error(
      submit_job(
        language               = NULL,
        shell_script_path      = NULL, 
        script_path            = "some/script/path/scriptname.R", 
        std_err_path           = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
        std_out_path           = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
        job_name               = NULL, 
        archiveTF              = FALSE,  
        mem_GB                 = "10G", 
        threads                = "2", 
        runtime_min            = "15", 
        partition              = "all.q", 
        Account                = NULL, 
        r_image                = NULL,  
        args_list              = NULL,
        dry_runTF              = TRUE
      ),
      regexp = "Input a valid language \\(case insensitive\\): r, python"
    )
    
    expect_error(
      submit_job(
        language               = "ruby",
        shell_script_path      = NULL, 
        script_path            = "some/script/path/scriptname.R", 
        std_err_path           = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
        std_out_path           = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
        job_name               = NULL, 
        archiveTF              = FALSE,  
        mem_GB                 = "10G", 
        threads                = "2", 
        runtime_min            = "15", 
        partition              = "all.q", 
        Account                = NULL, 
        r_image                = NULL,  
        args_list              = NULL,
        dry_runTF              = TRUE
      ),
      regexp = "Input a valid language \\(case insensitive\\): r, python"
    )
    
    expect_error(
      submit_job(
        language               = "R",
        shell_script_path      = NULL, 
        script_path            = "some/script/path/scriptname.R", 
        std_err_path           = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
        std_out_path           = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
        job_name               = NULL, 
        archiveTF              = FALSE,  
        mem_GB                 = "10G", 
        threads                = "2", 
        runtime_min            = "15", 
        partition              = "all.q", 
        Account                = NULL, 
        r_image                = NULL,  
        args_list              = NULL,
        dry_runTF              = TRUE
      ),
      regexp = "Please define a Slurm Account e.g. proj_cov_vc"
    )
  }
)


 test_that(
   "submit_job dry-run produces expected sbatch string",
   {
     expect_message(
       submit_job(
         language               = "R",
         shell_script_path      = NULL, 
         script_path            = "some/script/path/scriptname.R", 
         std_err_path           = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
         std_out_path           = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
         job_name               = NULL, 
         archiveTF              = FALSE,  
         mem_GB                 = "10G", 
         threads                = "2", 
         runtime_min            = "15", 
         partition              = "all.q", 
         Account                = "proj_cov_vc", 
         r_image                = NULL,  
         args_list              = NULL,
         dry_runTF              = TRUE
       ),
       regexp = "sbatch -J scriptname --mem=10G -c 2 -t 15 -p all.q -A proj_cov_vc -e /mnt/share/temp/slurmoutput/ssbyrne/error/%x_e%j.log -o /mnt/share/temp/slurmoutput/ssbyrne/output/%x_o%j.log /ihme/singularity-images/rstudio/shells/execRscript.sh -i /ihme/singularity-images/rstudio/latest.img -s some/script/path/scriptname.R"
     )
     
     expect_message(
       submit_job(
         language               = "R",
         shell_script_path      = NULL, 
         script_path            = "some/script/path/scriptname.R", 
         std_err_path           = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
         std_out_path           = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
         job_name               = NULL, 
         archiveTF              = FALSE,  
         mem_GB                 = "10G", 
         threads                = "2", 
         runtime_min            = "15", 
         partition              = "all.q", 
         Account                = "proj_cov_vc", 
         r_image                = NULL,  
         args_list              = list(a = 'arg_a',
                                       b = 'arg_b'),
         dry_runTF              = TRUE
       ),
       regexp = "sbatch -J scriptname --mem=10G -c 2 -t 15 -p all.q -A proj_cov_vc -e /mnt/share/temp/slurmoutput/ssbyrne/error/%x_e%j.log -o /mnt/share/temp/slurmoutput/ssbyrne/output/%x_o%j.log /ihme/singularity-images/rstudio/shells/execRscript.sh -i /ihme/singularity-images/rstudio/latest.img -s some/script/path/scriptname.R --a arg_a --b arg_b"
     )
   }
 )
