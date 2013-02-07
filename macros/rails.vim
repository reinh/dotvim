Rnavcommand config     config       -glob=*.*  -suffix= -default=routes.rb
Rnavcommand asset      app/assets   -glob=**/*
Rnavcommand job        app/jobs     -glob=**/* -suffix=.rb
Rnavcommand service    app/services -glob=**/* -suffix=_service.rb

Rnavcommand shared     spec/support/shared_examples -glob=**/* -suffix=.rb
Rnavcommand fabricator spec/fabricators -glob=**/* -suffix=_fabricator.rb
