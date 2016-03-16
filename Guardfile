guard :shell do
  watch(%r{test/.+\.l?hs$})
  watch(%r{src/.+\.l?hs$})
  watch(%r{\.cabal$})
  watch(%r{stack\.yaml})
  
  callback(:start_end) { `stack test` }
  callback(:reload) { 'stack test' }
  callback(:run_all_end) { `stack test` }
  callback(:run_on_changes_end) { `stack test` }
  callback(:run_on_additions_end) { `stack test` }
  callback(:run_on_modifications_end) { `stack test` }
end