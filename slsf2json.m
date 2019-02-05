function s = slsf2json(chart)

    children = find(chart,'-isa','Stateflow.State');
    transitions = find(chart,'-isa','Stateflow.Transition');
    s = jsonencode(buildHelper(chart));

    function s = buildHelper(state)

        s = struct();
        s.name = state.Name;
        s.children = {};
        for i=1:length(children)
            c = children(i);
            if strcmp(c.getParent.Name,state.Name)
                s.children{end+1} = buildHelper(c);
            end
        end

        if isempty(s.children)
            s.ty = "mode";
            s.flow = state.LabelString;
        else
            % differentiate between Parallel and Regular model
            if strcmp(s.children{1}.ty,"mode")
                s.ty = "regmodel";
            else
                s.ty = "parmodel";
            end
            s.decomposition = state.Decomposition;
            s.transitions = {};
            localtrans = find(transitions,'-function', ...
                @(t) strcmp(t.getParent.Name, state.Name));
            for j=1:length(localtrans)
                s.transitions{end+1} = convertTransition(localtrans(j));
            end
        end
    end

    function s = createMode(m)
        s.name = m.Name;
        s.ty = "mode";
        s.flow = m.LabelString;
    end

    function s = convertTransition(t)
        s = struct();
        s.guard = t.LabelString;
        if ~isempty(t.Source)
            s.src = createMode(get(t.Source));
        else
            m = struct();
            m.name = "InitialTransition";
            m.flow = "InitialTransition: ";
            m.ty = "mode";
            s.src = m;
        end
        s.dest = createMode(get(t.Destination));
    end
end