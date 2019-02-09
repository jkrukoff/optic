%%%-------------------------------------------------------------------
%%% @doc
%%% Macros for the optic_tuples module.
%%% @end
%%%-------------------------------------------------------------------

% This macro creates the first three parameters required by
% optic_tuple:field/3 for selecting the field of a record. It requires
% the record to be defined in the current module.
-define(OPTIC_FIELD(Record, Field), (Record), record_info(size, (Record)), (#Record.Field)).
