Attempting to finalize metadata for yap4py
INFO:conda_build.metadata:Attempting to finalize metadata for yap4py
Traceback (most recent call last):
  File "/home/vsc/anaconda3/bin/conda-build", line 11, in <module>
    sys.exit(main())
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda_build/cli/main_build.py", line 388, in main
    execute(sys.argv[1:])
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda_build/cli/main_build.py", line 379, in execute
    verify=args.verify)
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda_build/api.py", line 185, in build
    need_source_download=need_source_download, config=config, variants=variants)
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda_build/build.py", line 1774, in build_tree
    notest=notest,
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda_build/build.py", line 882, in build
    output_metas = expand_outputs([(m, need_source_download, need_reparse_in_env)])
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda_build/render.py", line 553, in expand_outputs
    for (output_dict, m) in _m.get_output_metadata_set(permit_unsatisfiable_variants=False):
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda_build/metadata.py", line 1767, in get_output_metadata_set
    permit_unsatisfiable_variants=permit_unsatisfiable_variants)
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda_build/metadata.py", line 640, in finalize_outputs_pass
    fm = finalize_metadata(om, permit_unsatisfiable_variants=permit_unsatisfiable_variants)
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda_build/render.py", line 290, in finalize_metadata
    permit_unsatisfiable_variants=permit_unsatisfiable_variants)
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda_build/render.py", line 113, in get_env_dependencies
    channel_urls=tuple(m.config.channel_urls))
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda_build/environ.py", line 683, in get_install_actions
    actions = install_actions(prefix, index, specs, force=True)
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda/plan.py", line 461, in install_actions
    update_deps, pinned)
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda/plan.py", line 631, in get_actions_for_dists
    pkgs = r.install(specs, installed, update_deps=update_deps)
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda/resolve.py", line 813, in install
    pkgs = self.solve(specs, returnall=returnall)
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda/resolve.py", line 853, in solve
    reduced_index = self.get_reduced_index(specs)
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda/resolve.py", line 437, in get_reduced_index
    found = filter_group([s])
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda/resolve.py", line 417, in filter_group
    res = filter_group(set(deps))
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda/resolve.py", line 417, in filter_group
    res = filter_group(set(deps))
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda/resolve.py", line 417, in filter_group
    res = filter_group(set(deps))
  [Previous line repeated 1 more times]
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda/resolve.py", line 391, in filter_group
    for ms in self.ms_depends(fkey)))
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda/resolve.py", line 391, in <genexpr>
    for ms in self.ms_depends(fkey)))
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda/resolve.py", line 390, in <genexpr>
    all(any(filter.get(f2, True) for f2 in self.find_matches(ms))
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda/models/dist.py", line 242, in __hash__
    return hash(self.__key__())
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda/models/dist.py", line 223, in __key__
    return self.channel, self.dist_name, self.with_features_depends
  File "/home/vsc/anaconda3/lib/python3.6/site-packages/conda/_vendor/auxlib/entity.py", line 417, in __get__
    return self.unbox(instance, instance_type, val)
KeyboardInterrupt
