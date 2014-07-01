#!/bin/sh
cat lambdabot.log >> lambdabot.log.old
LC_ALL=en_US.UTF-8 \
  cabal exec lambdabot -- \
  --trust=array \
  --trust=semigroups \
  --trust=base \
  --trust=bytestring \
  --trust=containers \
  --trust=lambdabot-trusted \
  --trust=lens \
  --trust=random \
  -X BangPatterns \
  -X ConstrainedClassMethods \
  -X DeriveDataTypeable \
  -X DeriveFoldable \
  -X DeriveFunctor \
  -X DeriveGeneric \
  -X DeriveTraversable \
  -X EmptyDataDecls \
  -X ExistentialQuantification \
  -X ExtendedDefaultRules \
  -X FlexibleContexts \
  -X FlexibleInstances \
  -X FunctionalDependencies \
  -X GADTs \
  -X ImplicitParams \
  -X ImplicitPrelude \
  -X KindSignatures \
  -X LiberalTypeSynonyms \
  -X MagicHash \
  -X MultiParamTypeClasses \
  -X PackageImports \
  -X ParallelListComp \
  -X PatternGuards \
  -X PolymorphicComponents \
  -X PostfixOperators \
  -X RankNTypes \
  -X ScopedTypeVariables \
  -X StandaloneDeriving \
  -X TupleSections \
  -X TypeOperators \
  -X TypeSynonymInstances \
  -X UnboxedTuples \
  -X UndecidableInstances \
  -X UnicodeSyntax \
  -X ViewPatterns \
  -X NoMonomorphismRestriction \
  -e 'rc scripts/online/saki.rc' \
  > lambdabot.log 2>&1
