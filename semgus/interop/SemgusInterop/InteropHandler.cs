using Semgus.Model;
using Semgus.Model.Smt;
using Semgus.Model.Smt.Terms;

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Semgus.Parser.Interop.CommonLisp
{
    public class InteropHandler : ISemgusProblemHandler
    {
        public InteropHandler()
        {
            _semgusContexts = new List<SemgusContext>();
        }

        public bool ParseSynthesisProblem(string filename)
        {
            using SemgusParser semgusParser = new(filename);
            return semgusParser.TryParse(this);
        }

        private readonly List<SemgusContext> _semgusContexts;
        public IReadOnlyList<SemgusContext> SemgusContexts => _semgusContexts;

        public void OnCheckSynth(SmtContext smtCtx, SemgusContext semgusCtx)
        {
            _semgusContexts.Add(semgusCtx);
        }

        public void OnConstraint(SmtContext smtCtx, SemgusContext semgusCxt, SmtTerm constraint)
        {
            
        }

        public void OnSetInfo(SmtContext ctx, SmtAttribute attr)
        {
            
        }

        public void OnSynthFun(SmtContext ctx, SmtIdentifier name, IList<SmtConstant> args, SmtSort sort)
        {
            
        }

        public void OnTermTypes(IReadOnlyList<SemgusTermType> termTypes)
        {
            
        }
    }
}
