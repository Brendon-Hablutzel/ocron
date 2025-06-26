# ocron

A feature-rich, safer cron alternative, written in OCaml.

## TODO

- [x] cron schedule expression parser
- [x] persistent config storage
- [x] CLI interface for managing jobs using [cmdliner](https://erratique.ch/software/cmdliner/doc/tutorial.html)
- [x] CI
- [x] functionality for computing execution times
- [ ] job execution daemon
- [ ] automated releases
- [ ] tests for cron schedule expression parser
- [ ] other trigger types
- [ ] robust logging
- [ ] persistent metrics storage
- [ ] figure out how environment config should work
- [ ] failure notifications (and for anomalies, e.g. unexpected stderr or long duration)
- [ ] task support
- [ ] thorough docs here in the README
- [ ] cli commands to get natural language descriptions of schedules
