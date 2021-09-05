
# antgreens

<!-- badges: start -->
<!-- badges: end -->

先前完成的蔬菜类产品质量安全中长期风险调查分析报告在结构设计与代码逻辑上存在着一些不足，因此借助编写 R 包的机会整理优化先前的代码，重新设计自动化报告参数结构，最终形成的 R 包就是 `antgreens_v0.2.0`。

`antgreens` 汲取 `ant` 的团队合作与不言放弃的精神和 `greens` 的蔬菜与安全的涵义，致敬为我国蔬菜食品安全做出共享的每一个人。\\(0\^◇\^0)/

## 安装方法

- Windows

  使用 `antgreens_0.2.0.zip`

  ```R
  install.packages("~/bin-pkg/path", repos = NULL)
  ```

- Mac OS / Linux

  使用 `antgreens_0.2.0.tar.gz`

  ```R
  install.packages("~/source-pkg/path", repos = NULL, type = "source")
  ```

## 主要功能

`antgreens` 内置一份蔬菜类报告模板，实质上是一个 `bookdown` 项目：

```markdown
- `\report_template`
	- `\Codes`
	- `\Data`
		- `\raw_data`
	- `\Report`
		- `\css`
			- dt_css_custom.css
			- style.css
		- `\latex`
			- preamble.tex
		- _bookdown.yml
		- _output.yml
		- book.bib
		- packages.bib
		- index.Rmd
		- 01-work-overview.Rmd
		- 02-residue-risk-ana.Rmd
		- 03-multi-residue-risk-ana.Rmd
```

你可以使用 `antgreens::use_template(proj_dir, proj_name)` 将该模板复制到指定的目录下，并重命名项目。

模板中的 `index.Rmd` 已经预先设定好了项目的基础配置项，添加了数据导入和处理代码块并写好了 `antgreens` 在该模板上的基本使用流程。如果数据种类与处理流程不变，该文件中的项目配置项和代码不应变动。

在报告中可以使用 `antgreens` 还提供的表格输出函数、绘图函数和文字函数来丰富报告的内容。其中表格输出函数利用 `DT` 包输出交互式表格并解决了表格标题无法交叉引用的问题；绘图函数利用 `echarts4r` 和 `ggplot2` 包分别输出交互式图片和静态图片，具体的参数含义和使用方法请见 `antgreens` 文档；文字函数能够利用输入数据输出一些定义好格式的文本，例如各年份检出率最大的 3 项及其检出率等。

如果要编译报告，则需要设定好报告的编译参数，例如数据文件路径、要分析的维度等。`antgreens` 包设定的默认编译参数可以在模板中 `index.Rmd` 文件的 YAML 头部看到，其中**被注释**的 `params` 项给出了全部默认编译参数及其默认值。为了防止编译函数传递的 `params` 变量与 `index.Rmd` 中的 `params` 项冲突，因此需要注释掉报告文件中的编译参数。除此之外，还可以使用 `update_params()` 输出默认的编译参数。

使用 `auto_report()` 函数编译报告。可以设定的参数有：

- `input_dir`：`bookdown` 项目所在的目录，默认为 `/Report`。这意味着你需要在报告模板的根目录下运行代码，如果你进入了 `bookdown` 项目目录，则需要手动指定该参数为当前目录 `.`。
- `params`：编译参数，默认为空列表。你可以在该处以列表项的形式指定自己设定的任何编译参数，但需要注意，如果使用了**相对路径**，该相对路径需要以 `input_dir` 为“当前目录”，而不是当前你的工作目录。
- `output_dir`：报告文件输出的目录，默认为 `_book`。与 `params` 中的相对路径相同，该目录所在路径同样是以 `input_dir` 作为“当前”目录。

## 报告项目结构

`antgreens` 的报告项目结构主要分为三部分：

- `\Codes`：存放报告中使用到的代码文件。报告模板会自动将该文件夹下以 `.R` 作为后缀名的代码文件全部导入。
- `\Data`：存放数据文件。其中**全部**原始数据文件需要存放到 `\raw_data`目录下，**阈值**数据文件需要命名为 `multi_dim_check.xlsx` 存放到 `\Data` 目录下。
- `\Report`：`bookdown` 项目文件夹。其中 `\css` 存放 CSS 样式表，`\latex` 存放 LaTeX Preamble 文件。

上述要求并不是需要严格遵守的，因为它们与 `antgreens` 包的工作流程没有任何关系。它们以代码的形式规定在模板文件中，例如代码的存放位置与 `index.Rmd` 中 `load_other_codes` 代码块有关，如果你想使用自己的项目结构，修改对应位置的代码即可。但还是推荐使用预定的项目结构，因为它们针对报告做了简化和优化。

## Auto-Report

### 主要过程

1. 读取原始数据文件（Excel），并将其转换为能快速读取的 CSV 文件，并将编码格式转换为 `UTF-8 BOM`（即可以在 Windows 上正常浏览的 UTF-8 编码）；
2. 构造编译环境，即将报告所需编译参数放到环境中：
   1. 读取原始数据文件（CSV），获取数据的年份跨度；
   2. 根据传入参数**更新**生成报告所需编译参数，例如 MRL 数据文件路径、代码文件路径、分析维度名称等；
3. 利用 `bookdown::render_book()` 生成报告。

### 报告参数说明

> 以下参数均有默认值。

| 参数               | 说明                                                         |
| ------------------ | ------------------------------------------------------------ |
| `cran_mirror`      | CRAN 镜像站网址，用于安装缺失的 R 包                         |
| `raw_data`         | 原始数据文件路径                                             |
| `mrl_data`         | MRL 数据文件路径                                             |
| `products`         | 蔬菜产品信息数据文件路径                                     |
| `pesticides`       | 农药信息数据文件路径                                         |
| `province`         | 省份信息数据文件路径                                         |
| `ana_threshold`    | 各维度组合的样本量阈值数据文件路径                           |
| `codes_dir`        | 代码文件所在目录                                             |
| `ana_dims_part`    | 部分维度（除了产品和药品两个维度之外）中英文对照列表，例如`list(year = "年份")` |
| `ana_dims_product` | 产品维度中文名                                               |
| `ana_dims_drug`    | 药品维度中文名                                               |
| `threshold_kw`     | 阈值数据文件中表示各维度组合样本量阈值的那一列的列名关键词   |
| `st_drug`          | 农药起始列列名，例如 `"甲胺磷"`                              |
| `ed_drug`          | 农药终止列列名，例如 `"二甲戊灵"`                            |
| `product_env`      | 是否为生产环境。如果为 `TRUE`，则不使用缓存，否则使用缓存加速报告的编译速度。 |
| `digits`           | 保留小数位数，默认为 `2`                                     |
| `colname_map`      | 列名映射列表，将作为变量名的英文列名转换为中文，便于展示。   |

### 使用方法

将报告参数传递给函数 `auto_report` 即可：

```R
params = list(
  # 覆盖默认参数
  raw_data = "../Data/vegetables_2022-2024.xlsx",
  mrl_data = "../MRL_2024.xlsx",
  product_env = TRUE,
  digits = 4,
  # 自定义参数
  new_param = TRUE
)
auto_report(input_dir = "./Report", params_list = params)
```
