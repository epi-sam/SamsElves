test_that(
  "submit_job input validation produces expected errors",
  {
    expect_error(
      submit_job_array(
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
      submit_job_array(
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
      regexp = "Input a valid language \\(case insensitive\\):"
    )

    expect_error(
      submit_job_array(
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
      regexp = "Input a valid language \\(case insensitive\\):"
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
      regexp = "Please define a Slurm Account e.g. proj_cov_vpd"
    )

    expect_error(
      submit_job_array(
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
        Account                = "proj_cov_vpd",
        r_image                = NULL,
        args_list              = NULL,
        array_first_task       = 1,
        array_n_tasks          = 10,
        hold_for_JobIDs        = 12345,
        dry_runTF              = TRUE
      ),
      regexp = "hold_for_JobIDs must be a simple integer vector"
    )

    expect_error(
      submit_job_array(
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
        Account                = "proj_cov_vpd",
        r_image                = NULL,
        args_list              = NULL,
        array_first_task       = 1,
        array_n_tasks          = 10,
        hold_for_JobIDs        = list(12345L),
        dry_runTF              = TRUE
      ),
      regexp = "hold_for_JobIDs must be a simple integer vector"
    )

    expect_error(
      submit_job_array(
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
        Account                = "proj_cov_vpd",
        r_image                = NULL,
        args_list              = NULL,
        array_first_task       = 1,
        array_n_tasks          = NULL,
        hold_for_JobIDs        = NULL,
        dry_runTF              = TRUE
      ),
      regexp = "Please define number of jobs per array"
    )
  }
)


test_that(
  "submit_job dry-run produces expected sbatch string",
  {
    expect_message(
      submit_job_array(
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
        Account                = "proj_cov_vpd",
        r_image                = NULL,
        args_list              = NULL,
        array_first_task       = 1,
        array_n_tasks          = 10,
        hold_for_JobIDs        = 12345L,
        dry_runTF              = TRUE
      ),
      regexp = "sbatch -J scriptname --mem=10G -c 2 -t 15 -p all.q -A proj_cov_vpd -e /mnt/share/temp/slurmoutput/ssbyrne/error/%x_e%j.log -o /mnt/share/temp/slurmoutput/ssbyrne/output/%x_o%j.log /ihme/singularity-images/rstudio/shells/execRscript.sh -i /ihme/singularity-images/rstudio/latest.img -s some/script/path/scriptname.R--array=1-10 --dependency=afterok:12345"
    )
  }
)
