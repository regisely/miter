# Passos para Corrigir os Erros Restantes

## 1. Primeiro: REGENERAR DOCUMENTAÇÃO (CRÍTICO!)

O check não regenerou a documentação porque a versão do roxygen2 estava diferente. Já corrigi o DESCRIPTION. Agora rode:

```r
# Fazer pull das últimas mudanças
git pull origin claude/package-docs-tests-011CUPzAQbFkT1SeBSu5euQF

# Regenerar NAMESPACE e arquivos .Rd
devtools::document()
```

Isso irá:
- Regenerar o NAMESPACE com todos os novos imports
- Atualizar icms_br.Rd com a documentação correta
- Resolver os warnings de "no visible global function"

## 2. Erros nos Testes que Permanecem

Depois de rodar `devtools::document()`, ainda haverá alguns erros de teste que precisam ser corrigidos. Aqui está o resumo:

### A. Problema com `holdout_time_split` quando sem grupos

**Erro**: `prop` must be a number, not an empty numeric vector

**Causa**: Quando não há IDs (grupos), o código tenta fazer `distinct(across(all_of(NULL)))` que falha.

**Arquivos afetados**:
- R/holdout-splits.R (linhas 87-92)
- R/cv-splits.R (similar)

### B. Problema com `generate_lags`

**Erro**: Colunas esperadas não estão sendo criadas

**Testes falhando**:
- test-utils.R linha 9: esperando "icms_lag1" mas não está sendo criado
- test-utils.R linha 21: esperando "icms_lag1", "icms_lag2", "icms_lag3"

**Provável causa**: A função `timetk::tk_augment_lags` pode estar nomeando as colunas de forma diferente.

### C. Problema com `rolling_cv`

**Erro**: `data` must be a data frame (rsplit espera data frame mas recebe algo diferente)

**Arquivos afetados**: R/cv-splits.R linha 256-303

## 3. Warnings Aceitáveis vs Críticos

### ✅ Warnings ACEITÁVEIS (pode enviar ao CRAN assim):

1. **S3 generic/method consistency**: Os métodos têm primeiro argumento diferente (`object` vs `data`) por design para S3 dispatch
2. **rsample::: usage**: Não há alternativa pública para essas funções internas
3. **xgboost requireNamespace**: Usado apenas em código condicional

### ⚠️ Warnings que DEVEM ser corrigidos:

1. **Missing documentation entries**: Faltam docs para:
   - as_miter_pred
   - cv_split
   - cv_time_split
   - holdout_time_split
   - miter_predict
   - nested_cv_time_split
   - rolling_cv

2. **Data codoc mismatches**: icms_br.Rd ainda mostra `uf` e `value` (será corrigido com devtools::document())

3. **Rd \usage sections**: Faltam documentar argumentos em holdout_split.Rd

4. **Test dependencies**: Testes usam 'doParallel' e 'plotly' sem declarar

## 4. Plano de Ação Recomendado

### Passo 1: Regenerar Documentação
```r
git pull
devtools::document()
```

### Passo 2: Rodar Check Novamente
```r
devtools::check()
```

### Passo 3: Análise
Depois disso, me mostre o output e posso:
- Corrigir os bugs nos testes
- Adicionar documentação faltante se necessário
- Ajustar o que mais for preciso

## 5. Status Atual

✅ **Corrigido**:
- Versão do RoxygenNote atualizada
- Imports adicionados ao R/0_imports.R
- Global variables declaradas
- library() calls removidos
- Coluna names nos testes atualizadas (state, icms)
- .Rbuildignore atualizado

⏳ **Pendente** (após devtools::document()):
- Verificar se imports foram aplicados ao NAMESPACE
- Verificar se icms_br.Rd foi atualizado
- Corrigir bugs de teste
- Adicionar documentação faltante

❌ **Bugs de Código** (precisam ser corrigidos):
- holdout_time_split com ids = NULL
- cv_time_split com rsplit
- generate_lags naming

---

**PRÓXIMA AÇÃO**: Rode `devtools::document()` e depois `devtools::check()` novamente, e me mostre o resultado!
